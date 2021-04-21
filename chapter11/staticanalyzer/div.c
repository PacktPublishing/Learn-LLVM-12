int divbyzero(int a, int b) {
  return a / b;
}

int bug() {
  return divbyzero(5, 0);
}
