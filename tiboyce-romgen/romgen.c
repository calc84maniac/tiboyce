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

#define MAX_VAR_SIZE 65513

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
		return false;

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

void write_error(char *filename, struct zip_t *zip) {
	if (zip != NULL) {
		printf("Could not write file %s to archive\n", filename);
		zip_close(zip);
	}
	else {
		printf("Could not write to file %s\n", filename);
	}
	exit(1);
}

uint32_t write_tifile(struct tifile *file, const char *extension, struct zip_t *zip) {
	char filename[13] = {0};
	strncpy(filename, file->data.name, sizeof(file->data.name));
	strncat(filename, extension, sizeof(filename)-1);
	printf("Writing AppVar %s (var length = %d bytes)\n", filename, file->data.var_length);

	size_t full_length = offset_of(file, data) + file->file_length + 2;
	struct tifile *newfile = realloc(file, full_length);
	if (newfile == NULL) {
		free(file);
		write_error(filename, zip);
	}
	file = newfile;

	file->data.data_length = file->data.data_length_2 = file->data.var_length + 2;
	uint8_t *var = (uint8_t*)(&file->data);
	uint16_t *checksum = (uint16_t*)(&file->data.var_data[file->data.var_length]);
	*checksum = 0;
	while (var < (uint8_t*)checksum)
		*checksum += *var++;

	if (zip != NULL) {
		if (zip_entry_open(zip, filename) < 0) {
			free(file);
			write_error(filename, zip);
		}
		if (zip_entry_write(zip, file, full_length) < 0) {
			free(file);
			zip_entry_close(zip);
			write_error(filename, zip);
		}
		free(file);
		uint32_t crc32 = zip_entry_crc32(zip);
		if (zip_entry_close(zip) < 0) {
			write_error(filename, zip);
		}
		return crc32;
	}
	else {
		FILE *out = fopen(filename, "wb");
		if (out == NULL) {
			free(file);
			write_error(filename, zip);
		}
		if (fwrite(file, full_length, 1, out) != 1) {
			free(file);
			fclose(out);
			write_error(filename, zip);
		}
		free(file);
		fclose(out);
		return 0;
	}
}

uint8_t *read_rom(char *filename, size_t *length_out) {
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

uint16_t get_page_length(uint8_t *page, size_t remaining) {
	uint16_t full_length = (remaining < 0x4000) ? (uint16_t)remaining : 0x4000;
	uint16_t page_length = full_length;
	uint8_t byte = page[--page_length];
	if (byte != 0x00 && byte != 0xFF)
		return page_length + 1;
	while (page_length > 0 && page[--page_length] == byte)
		;
	if (page[page_length] == byte)
		return 0;
	return min(page_length + 1 + 256, full_length);
}

int pageinfo_comparator(const void *p1, const void *p2) {
	return ((struct pageinfo *)p1)->length - ((struct pageinfo *)p2)->length;
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
	printf("Usage: %s [-b83 | -b84 | -z] [-t \"Game Title\"] romfile outname\n", file);
	printf("  -b83: Pack in TI-83 Premium CE bundle (requires TI-Connect CE 5.3)\n");
	printf("  -b84: Pack in TI-84 Plus CE bundle (requires TI-Connect CE 5.3)\n");
	printf("  -z: Pack in zip archive (must unzip before transferring)\n");
	//printf("  -u: Set output files to send as Unarchived\n");
	printf("  -t: The title to display in the ROM list (defaults to internal ROM name)\n");
	printf("  romfile: The path to the ROM file to split\n");
	printf("  outname: The prefix name to use for the output AppVar files\n");
	exit(1);
}

void exit_pause(void) {
	printf("\nPress Enter to exit ");
	getchar();
}

int main(int argc, char **argv) {
	char outname_prompt[7];
	char title_prompt[256];
	char *romfile = NULL;
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
				if (pack != PACK_NONE || (*arg)[2] != '8')
					usage(argv[0]);
				if ((*arg)[3] == '3') {
					pack = PACK_B83;
				}
				else if ((*arg)[3] == '4') {
					pack = PACK_B84;
				}
				else {
					usage(argv[0]);
				}
				break;
			case 't':
			case 'T':
				if (*++arg == NULL)
					usage(argv[0]);
				title = *arg;
				break;
			case 'u':
			case 'U':
				flag = VAR_UNARCHIVED;
				break;
			case 'z':
			case 'Z':
				if (pack != PACK_NONE)
					usage(argv[0]);
				pack = PACK_ZIP;
				break;
			default:
				usage(argv[0]);
			}
		}
		else if (romfile == NULL) {
			romfile = *arg;
		}
		else if (outname == NULL) {
			outname = *arg;
		}
		else {
			usage(argv[0]);
		}
	}
	if (romfile == NULL)
		usage(argv[0]);

	if (outname == NULL) {
		while (title == NULL)
		{
			printf("Enter game title to display in ROM list: ");
			title = &title_prompt[0];
			if (!fgets(title, sizeof(title_prompt), stdin))
				exit(1);

			size_t size = strlen(title);
			if (title[size - 1] == '\n') {
				title[size - 1] = '\0';
			}
			else {
				printf("Title is too long\n");
				title = NULL;
				int c;
				do {
					c = getchar();
					if (c < 0)
						exit(1);
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
				scanf("%d", &selection);
				int c;
				do {
					c = getchar();
					if (c < 0)
						exit(1);
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
				exit(1);

			size_t size = strlen(outname);
			if (outname[size - 1] == '\n') {
				outname[size - 1] = '\0';
			}
			else {
				int c;
				do {
					c = getchar();
					if (c < 0)
						exit(1);
				} while (c != '\n');
			}
		}

		if (strlen(outname) > 5) {
			printf("Output name must be at most 5 characters long\n");
			outname = NULL;
			continue;
		}

		if (outname[0] < 'A' || outname[0] > 'Z') {
			printf("Output name must begin with a capital letter\n");
			outname = NULL;
			continue;
		}

		for (size_t i = 1; outname[i] != '\0'; ++i) {
			if (!isalnum(outname[i])) {
				printf("Output name must be alphanumeric\n");
				outname = NULL;
				break;
			}
		}
	} while (outname == NULL);

	size_t rom_length;
	uint8_t *rom = read_rom(romfile, &rom_length);
	if (rom == NULL) {
		printf("Could not read rom file %s\n", romfile);
		exit(1);
	}

	if (rom_length < 0x150) {
		printf("Not a valid GB ROM\n");
		exit(1);
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

	struct pageinfo pages[128];
	uint8_t num_pages = 0;
	while (num_pages * 0x4000U < rom_length) {
		if (num_pages == 128) {
			printf("ROM is too large!\n");
			exit(1);
		}

		uint16_t page_length = get_page_length(&rom[num_pages * 0x4000], rom_length - num_pages * 0x4000);
		pages[num_pages].length = page_length + 3;
		pages[num_pages].index = (uint8_t)num_pages;
		num_pages++;
	}

	struct tifile *metadatafile = open_tifile(TYPE_APPVAR, outname, flag);
	append_tifile(&metadatafile, metaheader, sizeof(metaheader));
	append_tifile(&metadatafile, &num_pages, sizeof(num_pages));
	uint8_t title_length = (uint8_t)strlen(title);
	append_tifile(&metadatafile, &title_length, sizeof(title_length));
	append_tifile(&metadatafile, title, title_length);

	struct tifile *romfiles[256] = { NULL };
	int romfilecount = 0;
	while (num_pages > 0) {
		char appvarname[9];
		sprintf(appvarname, "%sR%02d", outname, romfilecount);
		struct tifile *romfile = open_tifile(TYPE_APPVAR, appvarname, flag);
		
		int pages_to_use = best_fit(pages, num_pages);
		if (pages_to_use <= 0) {
			printf("Error choosing pages for AppVar %s\n", appvarname);
			exit(1);
		}

		while (pages_to_use > 0) {
			pages_to_use--;
			num_pages--;
			uint8_t page_index = pages[num_pages].index;
			uint16_t page_length = pages[num_pages].length - 3;

			if (!append_tifile(&romfile, &page_index, sizeof(page_index))) {
				printf("Error appending page index to AppVar %s\n", appvarname);
				exit(1);
			}
			if (!append_tifile(&romfile, &page_length, sizeof(page_length))) {
				printf("Error appending page length to AppVar %s\n", appvarname);
				exit(1);
			}
			if (!append_tifile(&romfile, &rom[page_index * 0x4000], page_length)) {
				printf("Error appending page data to AppVar %s\n", appvarname);
				exit(1);
			}
		}

		romfiles[romfilecount++] = romfile;
	}

	printf("ROM AppVar count: %d\n", romfilecount + 1);

	struct zip_t *zip = NULL;
	if (pack != PACK_NONE) {
		char zipname[10] = {0};
		strncpy(zipname, outname, sizeof(zipname));
		if (pack == PACK_B83) {
			strncat(zipname, ".b83", sizeof(zipname)-1);
		}
		else if (pack == PACK_B84) {
			strncat(zipname, ".b84", sizeof(zipname)-1);
		}
		else {
			strncat(zipname, ".zip", sizeof(zipname)-1);
		}
		printf("Opening file %s for output\n", zipname);
		zip = zip_open(zipname, ZIP_DEFAULT_COMPRESSION_LEVEL, 'w');
		if (zip == NULL) {
			printf("Could not open file %s for writing\n", zipname);
			exit(1);
		}
	}

	uint32_t checksum = write_tifile(metadatafile, ".8xv", zip);
	for (int i = 0; i < romfilecount; i++) {
		checksum += write_tifile(romfiles[i], ".8xv", zip);
	}

	switch (pack) {
	case PACK_B83:
	case PACK_B84:
		printf("Writing bundle metadata\n");
		if (zip_entry_open(zip, "METADATA") < 0) {
			write_error("METADATA", zip);
		}
		if (zip_entry_write(zip,
				(pack == PACK_B83 ? b83metadata : b84metadata),
				(pack == PACK_B83 ? sizeof(b83metadata) : sizeof(b84metadata)) - 1) < 0) {
			zip_entry_close(zip);
			write_error("METADATA", zip);
		}
		checksum += zip_entry_crc32(zip);
		if (zip_entry_close(zip) < 0)
		{
			write_error("METADATA", zip);
		}

		printf("Writing bundle checksum\n");
		if (zip_entry_open(zip, "_CHECKSUM") < 0)
		{
			write_error("_CHECKSUM", zip);
		}
		char checksum_str[11];
		sprintf(checksum_str, "%08x\r\n", checksum);
		if (zip_entry_write(zip, checksum_str, 10) < 0)
		{
			zip_entry_close(zip);
			write_error("_CHECKSUM", zip);
		}
		if (zip_entry_close(zip) < 0)
		{
			write_error("_CHECKSUM", zip);
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