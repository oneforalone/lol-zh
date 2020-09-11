# -*- coding: utf-8 -*-

# translate.py

# Author: Yuqi Liu
# E-Mail: yuqi.lyle@outlook.com

# Description:
# A python script using Spider via Youdao
# translating English to Chinese.


import os.path
import time

from youdao_spider import YoudaoSpider


def translate(src, dest):
    print("Translating %s to %s" % (src, dest), end="")
    with open(src, "r", encoding="utf-8") as f:
        fd = open(dest, "a+", encoding="utf-8")
        for line in f.readlines():
            spider = YoudaoSpider(line.strip())
            res = spider.get_result()
            fd.write(res)
            print('.', end='')
            time.sleep(1)  # delay to avoid server's redirection
        fd.close()
    print("Completed!")

if __name__ == "__main__":
    pwd = os.path.abspath(os.path.dirname(__file__))

    for i in range(1, 9):
        src_file = os.path.join(pwd, "en/lol-chap-%d.txt" % i)
        dest_file = os.path.join(pwd, "zh/lol-chap-%d-zh.txt" % i)
        translate(src_file, dest_file)
