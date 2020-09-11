# -*- coding: utf-8 -*-

# formating.py

# Author: Yuqi Liu
# E-Mail: yuqi.lyle@outlook.com

# Description:
# Making text file to the standar format to get better
# translation support.

import os.path


def merge_lines2chap(src, dest):
    with open(src, "r", encoding="utf-8") as f:
        fd = open(dest, "a+", encoding="utf-8")
        for line in f.readlines():
            if line == "\n":
                fd.write(line)
            line = line.strip()
            if line.endswith("-"):
                line = line[:-1]
            else:
                line += " "
            fd.write(line.lstrip())
        fd.close()


def split_chap2file(src, dest_format):
    with open(src, "r", encoding="utf-8") as f:
        chap_num = 0
        fd = open((dest_format % chap_num), "a+", encoding="utf-8")
        for line in f.readlines():
            if line.startswith("Chapter"):
                fd.close()
                chap_num += 1
                chap_path = dest_format % chap_num
                fd = open(chap_path, "a+", encoding="utf-8")
            fd.write(line)
        fd.close()


if __name__ == "__main__":
    pwd = os.path.abspath(os.path.dirname(__file__))

    src = os.path.join(pwd, "lol.txt")
    dest = os.path.join(pwd, "lol.fmt.txt")
    merge_lines2chap(src, dest)

    src = dest
    dest_format = os.path.join(pwd, "en/lol-chap-%d.txt")
    split_chap2file(src, dest_format)
