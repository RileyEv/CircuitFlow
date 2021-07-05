# -*- coding: utf-8 -*-
import example
import os
import glob

def setup():
    files = glob.glob('./output/*')
    for f in files:
        os.remove(f)

def test_example_1(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(1,))
def test_example_10(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(10,))
def test_example_100(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(100,))
def test_example_200(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(200,))
def test_example_400(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(400,))
def test_example_600(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(600,))
def test_example_800(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(800,))
def test_example_1000(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(1000,))
def test_example_1200(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(1200,))
def test_example_1400(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(1400,))
def test_example_1600(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(1600,))
def test_example_1800(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(1800,))
def test_example_2000(benchmark):
    benchmark.pedantic(example.run, setup=setup, args=(2000,))


