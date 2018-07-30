#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lzf.h"

#define offset_of(STRUCTPTR, MEMBER) ((uint8_t*)(&(STRUCTPTR)->MEMBER) - (uint8_t*)(STRUCTPTR))

enum VAR_TYPE {
	TYPE_APPVAR = 0x15
};

enum VAR_FLAG {
	VAR_UNARCHIVED = 0x00,
	VAR_ARCHIVED = 0x80
};

enum FILE_TYPE {
	FILE_INVALID,
	FILE_SAV,
	FILE_8XV
};

#pragma pack(push, 1)
struct tidata {
	uint16_t data_length;
	uint16_t var_length;
	uint8_t var_data[0];
};

struct tivar {
	uint16_t header_length;
	uint16_t data_length;
	uint8_t type;
	char name[8];
	uint8_t version;
	uint8_t flag;
	struct tidata data;
};

struct tifile {
	char signature[11];
	uint8_t comment[42];
	uint16_t file_length;
	struct tivar var;
};
#pragma pack(pop)

struct tifile *open_tifile(enum VAR_TYPE type, const char *name, enum VAR_FLAG flag) {
	struct tifile *file = malloc(sizeof(struct tifile));
	if (file == NULL)
		return NULL;

	strcpy(file->signature, "**TI83F*\x1A\x0A");
	memset(file->comment, '\0', sizeof(file->comment));
	file->file_length = sizeof(file->var);

	file->var.header_length = (uint16_t)(offset_of(&file->var, data.data_length) - offset_of(&file->var, data_length));
	file->var.type = type;
	strncpy(file->var.name, name, sizeof(file->var.name));
	file->var.version = 0;
	file->var.flag = flag;
	file->var.data.var_length = 0;
	return file;
}

bool append_tifile(struct tifile **file, const void *data, size_t length) {
	size_t new_file_length = (*file)->file_length + length;
	if (new_file_length > 0xFFFC)
		return false;

	struct tifile *new_file = realloc(*file, offset_of(*file, var) + new_file_length);
	if (new_file == NULL)
		return false;

	size_t var_length = new_file->var.data.var_length;
	memcpy(&new_file->var.data.var_data[var_length], data, length);

	new_file->file_length = (uint16_t)new_file_length;
	new_file->var.data.var_length = (uint16_t)(var_length + length);
	*file = new_file;
	return true;
}

bool truncate_tifile(struct tifile *file, uint16_t length) {
	if (file->var.data.var_length < length)
		return false;

	file->file_length -= length;
	file->var.data.var_length -= length;
	return true;
}

void free_tifiles(struct tifile **files, int start, int end) {
	for (int i = start; i < end; i++) {
		free(files[i]);
	}
}

void write_error(const char *filename) {
	fprintf(stderr, "Could not write to file %s\n", filename);
}

bool write_tifile(const char *filename, struct tifile *file) {
	printf("Writing AppVar %s (var length = %d bytes)\n", filename, (int)file->var.data.var_length);

	size_t full_length = offset_of(file, var) + file->file_length + 2;
	struct tifile *newfile = realloc(file, full_length);
	if (newfile == NULL) {
		free(file);
		write_error(filename);
		return false;
	}
	file = newfile;

	file->var.data_length = file->var.data.data_length = file->var.data.var_length + 2;
	uint8_t *var = (uint8_t*)(&file->var);
	uint8_t *checksum_ptr = &file->var.data.var_data[file->var.data.var_length];
	uint16_t checksum = 0;
	while (var < checksum_ptr)
		checksum += *var++;
	checksum_ptr[0] = (checksum) & 0xFF;
	checksum_ptr[1] = (checksum >> 8) & 0xFF;

	FILE *out = fopen(filename, "wb");
	if (out == NULL) {
		free(file);
		write_error(filename);
		return false;
	}
	if (fwrite(file, full_length, 1, out) != 1) {
		free(file);
		fclose(out);
		write_error(filename);
		return false;
	}
	free(file);
	fclose(out);

	return true;
}

void *read_save_data(const char *filename, enum FILE_TYPE filetype, size_t *length_out) {
	printf("Opening save file %s\n", filename);

	FILE *file = fopen(filename, "rb");
	if (file == NULL) {
		fprintf(stderr, "Failed to open file %s\n", filename);
		return NULL;
	}

	if (fseek(file, 0, SEEK_END) != 0) {
		fclose(file);
		fprintf(stderr, "Failed to seek to end of file %s\n", filename);
		return NULL;
	}

	size_t length = ftell(file);
	rewind(file);

	uint8_t *filedata = malloc(length);
	if (filedata == NULL) {
		fclose(file);
		fprintf(stderr, "Failed to allocate memory for file %s\n", filename);
		return NULL;
	}

	if (fread(filedata, 1, length, file) != length) {
		free(filedata);
		fclose(file);
		fprintf(stderr, "Failed to read file %s\n", filename);
		return NULL;
	}

	fclose(file);

	if (filetype == FILE_SAV) {
		*length_out = length;
		return filedata;
	}

	if (length < sizeof(struct tifile)) {
		free(filedata);
		fprintf(stderr, "Header is missing from AppVar %s\n", filename);
		return NULL;
	}

	struct tifile *tifile = (struct tifile *)filedata;
	if (0 != strncmp(tifile->signature, "**TI83F*", 8)) {
		free(filedata);
		fprintf(stderr, "File signature invalid for AppVar %s\n", filename);
		return NULL;
	}

	if (length != offset_of(tifile, var) + tifile->file_length + 2U) {
		free(filedata);
		fprintf(stderr, "File length invalid for AppVar %s\n", filename);
		return NULL;
	}

	uint8_t *var = (uint8_t*)(&tifile->var);
	uint8_t *checksum_ptr = &filedata[length - 2];
	uint16_t checksum = 0;
	while (var < checksum_ptr)
		checksum += *var++;
	if (checksum != (checksum_ptr[0] | (checksum_ptr[1] << 8))) {
		free(filedata);
		fprintf(stderr, "File checksum invalid for AppVar %s\n", filename);
		return NULL;
	}

	if (tifile->file_length != tifile->var.header_length + tifile->var.data_length + 4) {
		free(filedata);
		fprintf(stderr, "Variable length invalid for AppVar %s\n", filename);
		return NULL;
	}

	struct tidata *tidata = (struct tidata *)((char *)&tifile->var.data_length + tifile->var.header_length);
	if (tidata->var_length != tidata->data_length - 2 || tidata->var_length != tifile->var.data_length - 2) {
		free(filedata);
		fprintf(stderr, "Variable length invalid for AppVar %s\n", filename);
		return NULL;
	}


	uint8_t *savedata = NULL;
	size_t savelength = 0;
	switch (tidata->var_data[0]) {
	case 0:
		savelength = tidata->var_length - 1;
		savedata = malloc(savelength);
		if (savedata == NULL) {
			free(filedata);
			fprintf(stderr, "Failed to allocate memory for AppVar %s", filename);
			return NULL;
		}
		memcpy(savedata, &tidata->var_data[1], savelength);
		break;
	case 1:
		savelength = tidata->var_data[1] | (tidata->var_data[2] << 8);
		savedata = malloc(savelength);
		if (savedata == NULL) {
			free(filedata);
			fprintf(stderr, "Failed to allocate memory for AppVar %s", filename);
			return NULL;
		}
		if (savelength != lzf_decompress(&tidata->var_data[3], tidata->var_length - 3, savedata, savelength)) {
			free(filedata);
			free(savedata);
			fprintf(stderr, "Failed to decompress save data from AppVar %s", filename);
			return NULL;
		}
		break;
	default:
		fprintf(stderr, "Invalid compression type %d", tidata->var_data[0]);
		break;
	}

	free(filedata);
	
	*length_out = savelength;
	return savedata;
}

bool write_save_data(const char *filename, enum FILE_TYPE filetype, char appvarname[8], enum VAR_FLAG varflag, void *savedata, size_t savedatalength) {
	if (filetype == FILE_SAV) {
		printf("Writing save file %s (length = %d bytes)\n", filename, (int)savedatalength);
		FILE *out = fopen(filename, "wb");
		if (out == NULL) {
			write_error(filename);
			return false;
		}
		if (fwrite(savedata, savedatalength, 1, out) != 1) {
			fclose(out);
			write_error(filename);
			return false;
		}
		fclose(out);

		return true;
	}
	
	void *compressed = malloc(savedatalength - 3);
	if (compressed == NULL) {
		fprintf(stderr, "Failed to allocate compression buffer for %s", filename);
		return false;
	}

	size_t compressedlength = lzf_compress(savedata, savedatalength, compressed, savedatalength - 3);

	struct tifile *tifile = open_tifile(TYPE_APPVAR, appvarname, varflag);
	if (tifile == NULL) {
		fprintf(stderr, "Failed to allocate AppVar for %s", filename);
	}
	if (compressedlength == 0) {
		uint8_t compression_type = 0;
		free(compressed);
		if (!append_tifile(&tifile, &compression_type, sizeof(compression_type))) {
			fprintf(stderr, "Failed to append compression type to AppVar %s", filename);
			return false;
		}
		if (!append_tifile(&tifile, savedata, savedatalength)) {
			fprintf(stderr, "Failed to append uncompressed save data to AppVar %s", filename);
			return false;
		}
	}
	else {
		uint8_t compression_type = 1;
		uint16_t uncompressedlength = (uint16_t)savedatalength;
		if (!append_tifile(&tifile, &compression_type, sizeof(compression_type))) {
			free(compressed);
			fprintf(stderr, "Failed to append compression type to AppVar %s", filename);
			return false;
		}
		if (!append_tifile(&tifile, &uncompressedlength, sizeof(uncompressedlength))) {
			free(compressed);
			fprintf(stderr, "Failed to append uncompressed size to AppVar %s", filename);
			return false;
		}
		if (!append_tifile(&tifile, compressed, compressedlength)) {
			free(compressed);
			fprintf(stderr, "Failed to append compressed save data to AppVar %s", filename);
			return false;
		}
		free(compressed);
	}

	return write_tifile(filename, tifile);
}

bool has_extension(const char *filename, const char *extension) {
	size_t name_len = strlen(filename);
	size_t ext_len = strlen(extension);
	if (name_len < ext_len) {
		return false;
	}

	for (size_t i = name_len - ext_len; i < name_len; i++) {
		if (tolower(filename[i]) != *extension++)
			return false;
	}
	return true;
}

enum FILE_TYPE get_file_type(const char *filename, char appvarname[8]) {
	if (has_extension(filename, ".sav") || has_extension(filename, ".srm")) {
		return FILE_SAV;
	}
	else if (has_extension(filename, ".8xv")) {
		if (appvarname != NULL) {
			size_t name_length;
			const char *name_start = strrchr(
				filename,
#ifdef _WIN32
				'\\');
#else
				'/');
#endif
			name_start = (name_start ? (name_start + 1) : filename);
			name_length = strlen(name_start) - 4;
			if (name_length < 3 || 0 != memcmp(&name_start[name_length - 3], "SAV", 3)) {
				fprintf(stderr, "AppVar name must end with SAV\n");
				return FILE_INVALID;
			}
			if (name_length < 4 || name_length > 8) {
				fprintf(stderr, "AppVar name must be between 4 and 8 characters\n");
				return FILE_INVALID;
			}
			if (name_start[0] < 'A' || name_start[0] > 'Z') {
				fprintf(stderr, "AppVar name must begin with an uppercase letter\n");
				return FILE_INVALID;
			}
			for (size_t i = 1; i < name_length - 3; i++) {
				if (!isalnum(name_start[i])) {
					fprintf(stderr, "AppVar name must be alphanumeric\n");
					return FILE_INVALID;
				}
			}
			memset(appvarname, 0, 8);
			memcpy(appvarname, name_start, name_length);
		}
		return FILE_8XV;
	}
	else
	{
		fprintf(stderr, "Unsupported file extension");
		return FILE_INVALID;
	}
}

void usage(char *file) {
	fprintf(stderr, "Usage: %s inputfile outputfile\n", file);
	fprintf(stderr, "  inputfile: May be a *.sav file, a *.srm file, or a *SAV.8xv file.\n");
	fprintf(stderr, "  outputfile: May be a *.sav file, a *.srm file, or a *SAV.8xv file.\n");
}

void exit_pause(void) {
	printf("\nPress Enter to exit ");
	(void)getchar();
}

int main(int argc, char **argv) {
	char outpath_prompt[FILENAME_MAX];
	char *inputpath = NULL;
	char *outputpath = NULL;
	enum VAR_FLAG flag = VAR_ARCHIVED;
	enum FILE_TYPE inputtype;
	enum FILE_TYPE outputtype;
	char output_appvarname[8];
	void *savedata;
	size_t savedatalength;

	(void)argc;

	for (char **arg = &argv[1]; *arg != NULL; arg++) {
		if ((*arg)[0] == '-') {
			char option = (*arg)[1];
			switch (option) {
			case 'u':
			case 'U':
				flag = VAR_UNARCHIVED;
				break;
			default:
				usage(argv[0]);
				return 1;
			}
		}
		else if (inputpath == NULL) {
			inputpath = *arg;
		}
		else if (outputpath == NULL) {
			outputpath = *arg;
		}
		else {
			usage(argv[0]);
			return 1;
		}
	}
	if (inputpath == NULL) {
		usage(argv[0]);
		return 1;
	}

	if (outputpath == NULL) {
		atexit(exit_pause);
	}

	inputtype = get_file_type(inputpath, output_appvarname);
	if (inputtype == FILE_INVALID) {
		return 1;
	}

	savedata = read_save_data(inputpath, inputtype, &savedatalength);
	if (savedata == NULL) {
		return 1;
	}

	if (savedatalength > 32768 + 48) {
		fprintf(stderr, "Save files larger than 32KB are not supported\n");
		return 1;
	}

	while (outputpath == NULL) {
		printf("Enter output path (*.sav, *.srm, or *SAV.8xv): ");
		outputpath = &outpath_prompt[0];
		if (!fgets(outpath_prompt, sizeof(outpath_prompt), stdin)) {
			free(savedata);
			return 1;
		}

		size_t size = strlen(outputpath);
		if (outputpath[size - 1] == '\n') {
			outputpath[size - 1] = '\0';
		}
		else {
			int c;
			do {
				c = getchar();
				if (c < 0)
					return 1;
			} while (c != '\n');
		}

		outputtype = get_file_type(outputpath, output_appvarname);
		if (outputtype == FILE_INVALID) {
			outputpath = NULL;
		}
	}

	outputtype = get_file_type(outputpath, output_appvarname);
	if (outputtype == FILE_INVALID) {
		free(savedata);
		return 1;
	}

	if (!write_save_data(outputpath, outputtype, output_appvarname, flag, savedata, savedatalength)) {
		free(savedata);
		return 1;
	}

	free(savedata);

	printf("Done!\n");

	return 0;
}