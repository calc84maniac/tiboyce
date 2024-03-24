#include <ctype.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "zip.h"

#undef min
#define min(a,b) (a) < (b) ? (a) : (b)

#define offset_of(STRUCTPTR, MEMBER) ((uint8_t*)(&(STRUCTPTR)->MEMBER) - (uint8_t*)(STRUCTPTR))

#define MAX_VAR_SIZE 65512

const uint8_t metaheader[8] = "TIBOYCE";

const char b83metadata[] = "bundle_identifier:TI Bundle\n"
                           "bundle_format_version:1\n"
                           "bundle_target_device:83CE\n"
                           "bundle_target_type:CUSTOM\n"
                           "bundle_comments:Created by tiboyce-romgen\n";

const char b84metadata[] = "bundle_identifier:TI Bundle\n"
                           "bundle_format_version:1\n"
                           "bundle_target_device:84CE\n"
                           "bundle_target_type:CUSTOM\n"
                           "bundle_comments:Created by tiboyce-romgen\n";

struct pageinfo {
	uint16_t length;
	uint8_t index;
};

enum VAR_TYPE {
	TYPE_APPVAR = 0x15
};

enum VAR_FLAG {
	VAR_UNARCHIVED = 0x00,
	VAR_ARCHIVED = 0x80
};

enum PACK_TYPE {
	PACK_NONE,
	PACK_B83,
	PACK_B84,
	PACK_ZIP
};

#pragma pack(push, 1)
struct tivar {
	uint16_t header_length;
	uint16_t data_length;
	uint8_t type;
	uint8_t name[8];
	uint8_t version;
	uint8_t flag;
	uint16_t data_length_2;
	uint16_t var_length;
	uint8_t var_data[0];
};

struct tifile {
	uint8_t signature[11];
	uint8_t comment[42];
	uint16_t file_length;
	struct tivar data;
};
#pragma pack(pop)

struct tifile *open_tifile(enum VAR_TYPE type, const char *name, enum VAR_FLAG flag) {
	struct tifile *file = malloc(sizeof(struct tifile));
	if (file == NULL)
		return NULL;

	strcpy(file->signature, "**TI83F*\x1A\x0A");
	memset(file->comment, '\0', sizeof(file->comment));
	file->file_length = sizeof(file->data);

	file->data.header_length = (uint16_t)(offset_of(&file->data, data_length_2) - offset_of(&file->data, data_length));
	file->data.type = type;
	strncpy(file->data.name, name, sizeof(file->data.name));
	file->data.version = 0;
	file->data.flag = flag;
	file->data.var_length = 0;
	return file;
}

bool append_tifile(struct tifile **file, const void *data, size_t length) {
	size_t new_file_length = (*file)->file_length + length;
	if (new_file_length > 0xFFFC)
		return false;

	struct tifile *new_file = realloc(*file, offset_of(*file, data) + new_file_length);
	if (new_file == NULL)
		return false;

	size_t var_length = new_file->data.var_length;
	memcpy(&new_file->data.var_data[var_length], data, length);

	new_file->file_length = (uint16_t)new_file_length;
	new_file->data.var_length = (uint16_t)(var_length + length);
	*file = new_file;
	return true;
}

bool truncate_tifile(struct tifile *file, uint16_t length) {
	if (file->data.var_length < length)
		return false;

	file->file_length -= length;
	file->data.var_length -= length;
	return true;
}

void free_tifiles(struct tifile **files, int start, int end) {
	for (int i = start; i < end; i++) {
		free(files[i]);
	}
}

struct tifile *create_metadata_file(const char *outname, const char *title, uint16_t num_pages, enum VAR_FLAG flag) {
	struct tifile *file = open_tifile(TYPE_APPVAR, outname, flag);
	if (file == NULL) {
		fprintf(stderr, "Error allocating memory for AppVar %s\n", outname);
		return NULL;
	}
	if (!append_tifile(&file, metaheader, sizeof(metaheader))) {
		fprintf(stderr, "Error appending header to AppVar %s\n", outname);
		free(file);
		return NULL;
	}
	uint8_t num_pages_byte = (uint8_t)num_pages;
	if (num_pages > 256 || !append_tifile(&file, &num_pages_byte, sizeof(num_pages_byte))) {
		fprintf(stderr, "Error appending page count to AppVar %s\n", outname);
		free(file);
		return NULL;
	}
	uint8_t title_length = (uint8_t)strlen(title);
	if (!append_tifile(&file, &title_length, sizeof(title_length))) {
		fprintf(stderr, "Error appending title length to AppVar %s\n", outname);
		free(file);
		return NULL;
	}
	if (!append_tifile(&file, title, title_length)) {
		fprintf(stderr, "Error appending title to AppVar %s\n", outname);
		free(file);
		return NULL;
	}
	return file;
}

void write_error(const char *filename, struct zip_t *zip) {
	if (zip != NULL) {
		fprintf(stderr, "Could not write file %s to archive\n", filename);
		zip_close(zip);
	}
	else {
		fprintf(stderr, "Could not write to file %s\n", filename);
	}
}

bool write_tifile(struct tifile *file, const char *extension, struct zip_t *zip, uint32_t *crc_accumulator) {
	char filename[13] = {0};
	strncpy(filename, file->data.name, sizeof(file->data.name));
	strcat(filename, extension);
	printf("Writing AppVar %s (var length = %d bytes)\n", filename, (int)file->data.var_length);

	size_t full_length = offset_of(file, data) + file->file_length + 2;
	struct tifile *newfile = realloc(file, full_length);
	if (newfile == NULL) {
		free(file);
		write_error(filename, zip);
		return false;
	}
	file = newfile;

	file->data.data_length = file->data.data_length_2 = file->data.var_length + 2;
	uint8_t *var = (uint8_t*)(&file->data);
	uint8_t *checksum_ptr = &file->data.var_data[file->data.var_length];
	uint16_t checksum = 0;
	while (var < checksum_ptr)
		checksum += *var++;
	checksum_ptr[0] = (checksum)      & 0xFF;
	checksum_ptr[1] = (checksum >> 8) & 0xFF;

	if (zip != NULL) {
		if (zip_entry_open(zip, filename) < 0) {
			free(file);
			write_error(filename, zip);
			return false;
		}
		if (zip_entry_write(zip, file, full_length) < 0) {
			free(file);
			zip_entry_close(zip);
			write_error(filename, zip);
			return false;
		}
		free(file);

		*crc_accumulator += zip_entry_crc32(zip);
		if (zip_entry_close(zip) < 0) {
			write_error(filename, zip);
			return false;
		}
	}
	else {
		FILE *out = fopen(filename, "wb");
		if (out == NULL) {
			free(file);
			write_error(filename, zip);
			return false;
		}
		if (fwrite(file, full_length, 1, out) != 1) {
			free(file);
			fclose(out);
			write_error(filename, zip);
			return false;
		}
		free(file);
		fclose(out);
	}

	return true;
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

uint8_t *read_rom(const char *filename, size_t *length_out) {
	if (has_extension(filename, ".zip")) {
		struct zip_t *zip = zip_open(filename, ZIP_DEFAULT_COMPRESSION_LEVEL, 'r');
		if (zip == NULL)
			return NULL;

		int n = zip_total_entries(zip);
		if (n < 0) {
			zip_close(zip);
			return NULL;
		}

		for (int i = 0; i < n; i++) {
			if (zip_entry_openbyindex(zip, i) < 0) {
				zip_close(zip);
				return NULL;
			}
			const char *name = zip_entry_name(zip);
			if (name == NULL) {
				zip_entry_close(zip);
				zip_close(zip);
			}
			if (has_extension(name, ".gb") || has_extension(name, ".gbc")) {
				void *rom = NULL;
				size_t length = 0;
				if (zip_entry_read(zip, &rom, &length) < 0) {
					zip_entry_close(zip);
					zip_close(zip);
					return NULL;
				}
				zip_entry_close(zip);
				zip_close(zip);
				*length_out = length;
				return (uint8_t *)rom;
			}
			if (zip_entry_close(zip) < 0) {
				zip_close(zip);
				return NULL;
			}
		}

		fprintf(stderr, "No .gb or .gbc file found in zip archive\n");
		return NULL;
	}
	else {
		FILE *file = fopen(filename, "rb");
		if (file == NULL)
			return NULL;

		if (fseek(file, 0, SEEK_END) != 0) {
			fclose(file);
			return NULL;
		}

		size_t length = ftell(file);
		rewind(file);

		uint8_t *rom = malloc(length);
		if (rom == NULL) {
			fclose(file);
			return NULL;
		}

		if (fread(rom, 1, length, file) != length) {
			free(rom);
			fclose(file);
			return NULL;
		}

		*length_out = length;
		fclose(file);
		return rom;
	}
}

uint16_t get_page_length(uint8_t *page, size_t remaining) {
	uint16_t full_length = (remaining < 0x4000) ? (uint16_t)remaining : 0x4000;
	uint16_t page_length = full_length;
	uint8_t trim_byte = page[page_length - 1];
	do {
		page_length--;
	} while (page_length > 0 && page[page_length - 1] == trim_byte);
	if (page_length == 0 && trim_byte == 0)
		return 0;
	return (page_length & 0xFF ? page_length | 0xFF : page_length) + 1;
}

int pageinfo_comparator(const void *p1, const void *p2) {
	const struct pageinfo *pageinfo1 = p1;
	const struct pageinfo *pageinfo2 = p2;
	int diff = pageinfo1->length - pageinfo2->length;
	if (diff != 0)
		return diff;
	return pageinfo1->index - pageinfo2->index;
}

int best_fit(struct pageinfo *pages, int num_pages) {
	qsort(pages, num_pages, sizeof(struct pageinfo), pageinfo_comparator);

	uint8_t (*subset_sum)[MAX_VAR_SIZE + 1] = calloc(num_pages + 1, sizeof(*subset_sum));
	if (subset_sum == NULL)
		return -1;

	subset_sum[0][0] = 1;
	for (int page = 0; page < num_pages; page++) {
		int length = pages[page].length;
		memcpy(&subset_sum[page + 1], &subset_sum[page], sizeof(*subset_sum));
		for (int sum = length; sum <= MAX_VAR_SIZE; sum++) {
			subset_sum[page + 1][sum] |= subset_sum[page][sum - length];
		}
	}

	int size = MAX_VAR_SIZE;
	while (!subset_sum[num_pages][size])
		size--;

#if _DEBUG
	printf("Max size found: %d\n", size);
	printf("Page indices: ");
#endif

	int count = 0;
	for (int page = num_pages - 1; page >= 0; page--) {
		int length = pages[page].length;
		if (size >= length && subset_sum[page][size - length]) {
#if _DEBUG
			printf("%02X,%04X ", pages[page].index, pages[page].length);
#endif
			count++;
			size -= length;
			struct pageinfo temp = pages[num_pages - count];
			pages[num_pages - count] = pages[page];
			pages[page] = temp;
		}
	}
#if _DEBUG
	printf("\n");
#endif

	free(subset_sum);

	return count;
}

void usage(char *file) {
	fprintf(stderr, "Usage: %s [-b83 | -b84 | -z] [-t \"Game Title\"] romfile outname\n", file);
	fprintf(stderr, "  -b83: Pack in TI-83 Premium CE bundle (requires TI-Connect CE 5.3)\n");
	fprintf(stderr, "  -b84: Pack in TI-84 Plus CE bundle (requires TI-Connect CE 5.3)\n");
	fprintf(stderr, "  -z: Pack in zip archive (must unzip before transferring)\n");
	//fprintf(stderr, "  -u: Set output files to send as Unarchived\n");
	fprintf(stderr, "  -t: The title to display in the ROM list (defaults to internal ROM name)\n");
	fprintf(stderr, "  romfile: The path to the ROM file to split\n");
	fprintf(stderr, "  outname: The prefix name to use for the output AppVar files\n");
}

void exit_pause(void) {
	printf("\nPress Enter to exit ");
	(void)getchar();
}

int main(int argc, char **argv) {
	char outname_prompt[7];
	char title_prompt[256];
	char *romfilename = NULL;
	char *outname = NULL;
	char *title = NULL;
	enum VAR_FLAG flag = VAR_ARCHIVED;
	enum PACK_TYPE pack = PACK_NONE;

	for (char **arg = &argv[1]; *arg != NULL; arg++) {
		if ((*arg)[0] == '-') {
			char option = (*arg)[1];
			switch (option) {
			case 'b':
			case 'B':
				if (pack != PACK_NONE || (*arg)[2] != '8') {
					usage(argv[0]);
					return 1;
				}
				if ((*arg)[3] == '3') {
					pack = PACK_B83;
				}
				else if ((*arg)[3] == '4') {
					pack = PACK_B84;
				}
				else {
					usage(argv[0]);
					return 1;
				}
				break;
			case 't':
			case 'T':
				if (*++arg == NULL) {
					usage(argv[0]);
					return 1;
				}
				title = *arg;
				break;
			case 'u':
			case 'U':
				flag = VAR_UNARCHIVED;
				break;
			case 'z':
			case 'Z':
				if (pack != PACK_NONE) {
					usage(argv[0]);
					return 1;
				}
				pack = PACK_ZIP;
				break;
			default:
				usage(argv[0]);
				return 1;
			}
		}
		else if (romfilename == NULL) {
			romfilename = *arg;
		}
		else if (outname == NULL) {
			outname = *arg;
		}
		else {
			usage(argv[0]);
			return 1;
		}
	}
	if (romfilename == NULL) {
		usage(argv[0]);
		return 1;
	}

	if (outname == NULL) {
		while (title == NULL)
		{
			printf("Enter game title to display in ROM list: ");
			title = &title_prompt[0];
			if (!fgets(title, sizeof(title_prompt), stdin))
				return 1;

			size_t size = strlen(title);
			if (title[size - 1] == '\n') {
				title[size - 1] = '\0';
			}
			else {
				fprintf(stderr, "Title is too long\n");
				title = NULL;
				int c;
				do {
					c = getchar();
					if (c < 0)
						return 1;
				} while (c != '\n');
			}
		}
		if (title[0] == '\0')
		{
			printf("Defaulting to internal ROM title\n");
			title = NULL;
		}

		if (pack == PACK_NONE) {
			int selection = 0;
			while (selection < 1 || selection > 4) {
				printf("\n1: AppVar files (*.8xv)\n");
				printf("2: TI-83 Premium CE bundle (*.b83) - requires TI-Connect CE 5.3\n");
				printf("3: TI-84 Plus CE bundle (*.b84) - requires TI-Connect CE 5.3\n");
				printf("4: Zip archive (*.zip) - must extract before sending\n");
				printf("Choose an output format by number: ");
				if (scanf("%d", &selection) != 1) {
					return 1;
				}
				int c;
				do {
					c = getchar();
					if (c < 0)
						return 1;
				} while (c != '\n');
			}
			pack = (enum PACK_TYPE)(selection-1);
		}

		atexit(exit_pause);
	}

	do {
		if (outname == NULL) {
			printf("Enter output prefix name (5 chars max): ");
			outname = &outname_prompt[0];
			if (!fgets(outname, sizeof(outname_prompt), stdin))
				return 1;

			size_t size = strlen(outname);
			if (outname[size - 1] == '\n') {
				outname[size - 1] = '\0';
			}
			else {
				int c;
				do {
					c = getchar();
					if (c < 0)
						return 1;
				} while (c != '\n');
			}
		}

		if (strlen(outname) > 5) {
			fprintf(stderr, "Output name must be at most 5 characters long\n");
			outname = NULL;
			continue;
		}

		if (outname[0] < 'A' || outname[0] > 'Z') {
			fprintf(stderr, "Output name must begin with a capital letter\n");
			outname = NULL;
			continue;
		}

		for (size_t i = 1; outname[i] != '\0'; ++i) {
			if (!isalnum(outname[i])) {
				fprintf(stderr, "Output name must be alphanumeric\n");
				outname = NULL;
				break;
			}
		}
	} while (outname == NULL);

	size_t rom_length;
	uint8_t *rom = read_rom(romfilename, &rom_length);
	if (rom == NULL) {
		fprintf(stderr, "Could not read rom from file %s\n", romfilename);
		return 1;
	}

	if (rom_length < 0x150) {
		fprintf(stderr, "Not a valid GB ROM\n");
		free(rom);
		return 1;
	}

	char default_title[16];
	strncpy(default_title, &rom[0x0134], sizeof(default_title));
	default_title[sizeof(default_title) - 1] = '\0';
	printf("Opened ROM: %s\n", default_title);

	if (title == NULL || title[0] == '\0') {
		title = default_title;
	}

	for (char *c = title; *c != '\0'; c++) {
		if (*c < ' ' || *c > 0x7F) {
			*c = '?';
		}
	}

	struct pageinfo pages[256];
	uint16_t num_pages = 0;
	while (num_pages * 0x4000U < rom_length) {
		if (num_pages == 256) {
			fprintf(stderr, "ROM is too large!\n");
			free(rom);
			return 1;
		}

		uint16_t page_length = get_page_length(&rom[num_pages * 0x4000], rom_length - num_pages * 0x4000);
		pages[num_pages].length = page_length + 3;
		pages[num_pages].index = (uint8_t)num_pages;
		num_pages++;
	}

	struct tifile *romfiles[256] = { NULL };
	int romfilecount = 0;
	int pages_remaining = num_pages;
	while (pages_remaining > 0) {
		char appvarname[9];
		sprintf(appvarname, "%sR%02d", outname, romfilecount);
		struct tifile *romfile = open_tifile(TYPE_APPVAR, appvarname, flag);
		if (romfile == NULL) {
			fprintf(stderr, "Error allocating memory for AppVar %s\n", appvarname);
			free_tifiles(romfiles, 0, romfilecount);
			free(rom);
			return 1;
		}

		int pages_to_use = best_fit(pages, pages_remaining);
		if (pages_to_use <= 0) {
			fprintf(stderr, "Error choosing pages for AppVar %s\n", appvarname);
			free_tifiles(romfiles, 0, romfilecount);
			free(romfile);
			free(rom);
			return 1;
		}

		while (pages_to_use > 0) {
			pages_to_use--;
			pages_remaining--;
			uint8_t page_index = pages[pages_remaining].index;
			uint16_t page_length = pages[pages_remaining].length - 3;

			if (!append_tifile(&romfile, &page_index, sizeof(page_index))) {
				fprintf(stderr, "Error appending page index to AppVar %s\n", appvarname);
				free_tifiles(romfiles, 0, romfilecount);
				free(romfile);
				free(rom);
				return 1;
			}
			if (!append_tifile(&romfile, &page_length, sizeof(page_length))) {
				fprintf(stderr, "Error appending page length to AppVar %s\n", appvarname);
				free_tifiles(romfiles, 0, romfilecount);
				free(romfile);
				free(rom);
				return 1;
			}
			if (!append_tifile(&romfile, &rom[page_index * 0x4000], page_length)) {
				fprintf(stderr, "Error appending page data to AppVar %s\n", appvarname);
				free_tifiles(romfiles, 0, romfilecount);
				free(romfile);
				free(rom);
				return 1;
			}
		}

		romfiles[romfilecount++] = romfile;
	}

	free(rom);

	struct tifile *metadatafile = create_metadata_file(outname, title, num_pages, flag);
	if (metadatafile == NULL) {
		free_tifiles(romfiles, 0, romfilecount);
		return 1;
	}

	printf("ROM AppVar count: %d\n", romfilecount + 1);

	struct zip_t *zip = NULL;
	if (pack != PACK_NONE) {
		char zipname[10] = {0};
		strncpy(zipname, outname, sizeof(zipname));
		if (pack == PACK_B83) {
			strcat(zipname, ".b83");
		}
		else if (pack == PACK_B84) {
			strcat(zipname, ".b84");
		}
		else {
			strcat(zipname, ".zip");
		}
		printf("Opening file %s for output\n", zipname);
		zip = zip_open(zipname, ZIP_DEFAULT_COMPRESSION_LEVEL, 'w');
		if (zip == NULL) {
			fprintf(stderr, "Could not open file %s for writing\n", zipname);
			free_tifiles(romfiles, 0, romfilecount);
			free(metadatafile);
			return 1;
		}
	}

	uint32_t checksum = 0;
	if (!write_tifile(metadatafile, ".8xv", zip, &checksum)) {
		free_tifiles(romfiles, 0, romfilecount);
		return 1;
	}
	for (int i = 0; i < romfilecount; i++) {
		if (!write_tifile(romfiles[i], ".8xv", zip, &checksum)) {
			free_tifiles(romfiles, i + 1, romfilecount);
			return 1;
		}
	}

	switch (pack) {
	case PACK_B83:
	case PACK_B84:
		printf("Writing bundle metadata\n");
		if (zip_entry_open(zip, "METADATA") < 0) {
			write_error("METADATA", zip);
			return 1;
		}
		if (zip_entry_write(zip,
				(pack == PACK_B83 ? b83metadata : b84metadata),
				(pack == PACK_B83 ? sizeof(b83metadata) : sizeof(b84metadata)) - 1) < 0) {
			zip_entry_close(zip);
			write_error("METADATA", zip);
			return 1;
		}
		checksum += zip_entry_crc32(zip);
		if (zip_entry_close(zip) < 0)
		{
			write_error("METADATA", zip);
			return 1;
		}

		printf("Writing bundle checksum\n");
		if (zip_entry_open(zip, "_CHECKSUM") < 0)
		{
			write_error("_CHECKSUM", zip);
			return 1;
		}
		char checksum_str[11];
		int checksum_str_len = sprintf(checksum_str, "%x\r\n", checksum);
		if (zip_entry_write(zip, checksum_str, checksum_str_len) < 0)
		{
			zip_entry_close(zip);
			write_error("_CHECKSUM", zip);
			return 1;
		}
		if (zip_entry_close(zip) < 0)
		{
			write_error("_CHECKSUM", zip);
			return 1;
		}

		/*FALLTHROUGH*/
	case PACK_ZIP:
		zip_close(zip);
		break;

	default:
		break;
	}

	printf("Done!\n");

	return 0;
}