#include <gtest/gtest.h>

unsigned long fib (int rot);

TEST(FibonacciTest, ZeroToPositive) {
  ASSERT_EQ(fib(0), 0);
  ASSERT_EQ(fib(1), 1);
  ASSERT_EQ(fib(2), 1);
  ASSERT_EQ(fib(42), 267914296);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
