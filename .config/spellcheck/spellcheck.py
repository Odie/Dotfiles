#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["symspellpy"]
# ///

from symspellpy.symspellpy import SymSpell, Verbosity
import sys
import os


def load_spellchecker():
    sym_spell = SymSpell(max_dictionary_edit_distance=2)
    dictionary_path = os.path.expanduser(
        "~/.config/spellcheck/frequency_dictionary_en_82_765.txt"
    )
    sym_spell.load_dictionary(dictionary_path, term_index=0, count_index=1)
    return sym_spell


def suggest(word):
    sym_spell = load_spellchecker()
    suggestions = sym_spell.lookup(word, Verbosity.CLOSEST, max_edit_distance=2)
    return [s.term for s in suggestions[:5]]


def main():
    if len(sys.argv) < 2:
        print("")
        return
    word = sys.argv[1]
    results = suggest(word)
    print(";;".join(results))


if __name__ == "__main__":
    main()
