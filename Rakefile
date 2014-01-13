require("colorize")
require("pathname")

task :default => [:install]

# Creates a cooresponding symlink in targetDir for each file found in rootDir
# Returns a list of directories encountered but were not linked
def linkDirContents(rootDir, targetDir, linkDirs = false)
	dirs = Array.new
	rootDir = Pathname.new(rootDir)
	targetDir = Pathname.new(targetDir)

	filePattern = File.join(rootDir, "{*,.*}")

	Dir.glob(filePattern) { |item|
		# Filter out any items that are . or ..
		basename = File.basename(item)
		if basename == "." or basename == ".."
			next
		end

		# Skip any .git directory
		if basename == ".git"
			next
		end

		srcFullPath = Pathname.new(item)
		targetFullPath = targetDir + srcFullPath.relative_path_from(rootDir)

		# Skip this rake file
		if srcFullPath.to_s == __FILE__
			next
		end

		# Skip directory itself, but collect them to be returned
		if File.directory?(srcFullPath)
			if not linkDirs
				dirs.push(item)
				next
			end
		end

		# Link all individual files
		FileUtils.ln_sf(srcFullPath, targetFullPath)
		puts("#{srcFullPath} => #{targetFullPath}")
	}

	return dirs
end

task :install do
	puts("Installing dotfiles...".green)

	rootDir = Pathname.new(File.dirname(__FILE__))
	targetDir = Pathname.new(Dir.home)

	# Link the contents of dotfile/* to ~
	dirs = linkDirContents(rootDir, targetDir)

	# Now link the contents of dotfile/{dir}/* to ~/{dir}/*
	# This is done so we essentially merge with the contents of the top level home directories
	# instead of replacing them.
	dirs.each{|absItemPath|
		childRootDir = absItemPath
		childTargetDir = targetDir + Pathname.new(absItemPath).relative_path_from(rootDir)
		FileUtils.mkdir_p(childTargetDir)
		linkDirContents(childRootDir, childTargetDir, true)
	}

	puts("Done!".green)
end
