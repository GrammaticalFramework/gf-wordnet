#!/usr/bin/env python3
"""Transliterate text read from standard input."""

from pathlib import Path
import sys
import unicodedata


def load_table(path: Path) -> dict[int, str]:
    table: dict[int, str] = {}

    with path.open(encoding="utf-8") as source:
        for line in source:
            fields = line.split()
            character = fields[0]
            replacement = fields[1] if len(fields) > 1 else ""
            table[ord(character)] = replacement

            uppercase = character.upper()
            if uppercase != character and len(uppercase) == 1:
                table[ord(uppercase)] = replacement[:1].upper() + replacement[1:]

    return table


def main() -> None:
    table = load_table(Path(__file__).with_name("translit.txt"))
    text = sys.stdin.read()
    normalized = unicodedata.normalize("NFD", text)
    transliterated = normalized.translate(table)
    sys.stdout.write(text if transliterated == normalized else transliterated)


if __name__ == "__main__":
    main()
