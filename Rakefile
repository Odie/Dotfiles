require("colorize")
require("pathname")
require("find")

task :default => [:install]

def createSymlink(srcPath, targetPath)
  puts "#{srcPath} => #{targetPath}"
  FileUtils.ln_sf(srcPath, targetPath)
end

# Recreates the directory structure from rootDir in targetDir.
# Any files found within will be symlinked
#
# Any top level files or directory starting with "_" will be copied to
# its corresponding "." location.  Directories specified this way
# will be symlinked directly and will not have any of its contents
# visited recursively.
#
# With this, we can manage dotfile contents in two different ways:
# 1) Files we want to strategically placed into another directory structure
#    For example, if you're using spacemacs, you will only want to copy/link
#    over the snippet files.
#
# 2) Directories we want to have directly linked to within dotfiles
#    For example, you may want to manually manage .vim files and have all
#    changes automatically reflected in this dotfiles repo.
#
def linkDirContents(rootDir, targetDir)
	rootDir = Pathname.new(rootDir)
	targetDir = Pathname.new(targetDir)

	# Recurisvely visit all files in the rootDir
	Find.find(rootDir.to_s) { |srcFullPath|
		srcFullPath = Pathname.new(srcFullPath)
		relativePath = srcFullPath.relative_path_from(rootDir)

		case relativePath.to_s
		when '.'
			next

		when '.DS_Store'
			next

		# ignore git directory and children
		when '.git'
			Find.prune
		end

		# Skip .git* files meant to be settings for this repo
		next if relativePath.to_s.start_with? ".git"

		# Skip this rake file
		next if srcFullPath.to_s == __FILE__


		if relativePath.to_s.start_with? "_"
			remappedPath = relativePath.dup.to_s
			remappedPath[0] = '.'

			targetFullPath = targetDir + remappedPath

			if targetFullPath.directory?
				FileUtils.rm_f(targetFullPath)
			end

			createSymlink(srcFullPath, targetFullPath)
			Find.prune
			next
		else
			targetFullPath = targetDir + relativePath
		end

		# Copy symlinks directly
		if srcFullPath.symlink?
			linkTarget = srcFullPath.readlink
			puts "Copying symlink: #{targetFullPath} => #{linkTarget}"
			FileUtils.rm_f(targetFullPath)
			`cp -R #{srcFullPath} #{targetFullPath}`
			next
		end

		# Recreate any directory structures
		if srcFullPath.directory?
			puts "making dir: #{targetFullPath}"
			FileUtils.mkdir_p(targetFullPath)
			next
		end

		# Create the symlink
		createSymlink(srcFullPath, targetFullPath)
	}
end

task :install do
	puts("Installing dotfiles...".green)

	rootDir = Pathname.new(File.dirname(__FILE__))
  targetDir = Pathname.new(Dir.home)
  # targetDir = Pathname.new("../test")

	linkDirContents(rootDir, targetDir)

	puts("\nDone!".green)
end
