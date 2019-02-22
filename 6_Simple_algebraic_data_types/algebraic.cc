#include <iostream>
#define _PI 3.14159265359

using namespace std;

class Shape {
    public:
    virtual ~Shape() {}
    virtual double area() = 0;
    virtual double circ() = 0;
};

class Circle: public Shape {

    double r;

    public:
    Circle(double r): r(r) {}

    double area() { return _PI * r * r; }
    double circ() { return 2 * _PI * r; }
};

class Rect: public Shape {

    double w, h;

    public:
    Rect(double w, double h): w(w), h(h) {}

    double area() { return w * h; }
    double circ() { return 2 * (w + h); }
};

class Square: public Rect {

    public:
    Square(double s): Rect(s,s) {}
};

int main() {
    Shape* s = new Circle(5);
    cout << s->area() << endl << s->circ() << endl;
    return 0;
}
