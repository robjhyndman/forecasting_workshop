default: targets

targets:
	Rscript -e "targets::tar_make()"

clean:
	Rscript -e "targets::tar_destroy()"
	rm -rf *_cache
	rm -rf *_files
