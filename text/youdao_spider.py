# -*- coding: utf-8 -*-

# youdao_spider.py

# Author: Yuqi Liu
# E-Mail: yuqi.lyle@outlook.com

# Description: Web Spider of Youdao Fanyi, simulated with website
# translating, without using Youdao Fanyi API.
#
# Problems: can not tranlating words endlessly, will be redirected
# after translating 10000 appromiately.

import time
import random
import hashlib
import requests

import json

class YoudaoSpider:
    def __init__(self, text):
        self.text = text
        self.url = "http://fanyi.youdao.com/translate?smartresult=dict&smartresult=rule"
        self.key = "]BjuETDhU)zqSxf-=B#7m" # key from youdao.min.js
        self.salt = self.get_salt()
        self.sign = self.get_sign()
        self.lts = self.get_lts()

    timestamp = int(time.time() * 10000)

    def encrypt(self, data):
        m = hashlib.md5()
        m.update(data.encode("utf-8"))
        return m.hexdigest()

    def get_salt(self):
        s = self.timestamp + random.randint(0, 10)
        return str(s)

    def get_sign(self):
        data = "fanyideskweb" + self.text + self.salt + self.key
        return self.encrypt(data)

    def get_lts(self):
        return str(self.timestamp)

    def do_payload(self):
        headers = {
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.83 Safari/537.36",
            "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
            "Referer": "http://fanyi.youdao.com/",
            "Cookie": "OUTFOX_SEARCH_USER_ID=1058170278@10.108.160.208; OUTFOX_SEARCH_USER_ID_NCOO=1123758786.456823; UM_distinctid=1747707018b141-07cafc16e8c693-f7b1332-1fa400-1747707018c32c; JSESSIONID=aaa06BYKfbbGMgCuwM3rx; ___rl__test__cookies=1599740320401"
        }

        payloads = {
            "i": self.text,
            "from": "AUTO",
            "to": "zh-CHS",
            "smartresult": "dict",
            "client": "fanyideskweb",
            "salt": self.salt,
            "sign": self.sign,
            "lts": self.lts,
            "bv": "e915c77f633538e8cf44c657fe201ebb",
            "doctype": "json",
            "version": "2.1",
            "keyfrom": "fanyi.web",
            "action": "FY_BY_CLICKBUTTION",
        }

        return requests.post(url=self.url, data=payloads, headers=headers).text

    def get_result(self):
        res = self.do_payload()
        if res.find("null") >= 0:
            return "\n"

        res = json.loads(res)
        text = ""
        if "translateResult" in res:
            res = res["translateResult"][0]
            for r in res:
                text += r["tgt"]
        text += "\n"
        return text


if __name__ == "__main__":
    pass
