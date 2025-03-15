#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <stdexcept>
using namespace std;

class BigInt {
    string digits; // Stored in reverse order (least significant digit first)

public:
    // Constructors
    BigInt(unsigned long long n = 0) {
        do {
            digits.push_back(n % 10);
            n /= 10;
        } while (n > 0);
    }

    BigInt(const string& s) {
        size_t first_non_zero = s.find_first_not_of('0');
        if (first_non_zero == string::npos) {
            digits = "0";
            return;
        }
        string valid_part = s.substr(first_non_zero);
        
        for (int i = valid_part.size()-1; i >= 0; i--) {
            if (!isdigit(valid_part[i]))
                throw invalid_argument("Non-digit character in string");
            digits.push_back(valid_part[i] - '0');
        }
    }

    BigInt(const BigInt& other) : digits(other.digits) {}

    // Helper functions
    bool is_zero() const {
        return digits.size() == 1 && digits[0] == 0;
    }

    void remove_leading_zeros() {
        while (digits.size() > 1 && digits.back() == 0)
            digits.pop_back();
    }

    int length() const { return digits.size(); }

    // Arithmetic operations
    BigInt& operator+=(const BigInt& other) {
        int carry = 0;
        int max_len = max(length(), other.length());
        
        for (int i = 0; i < max_len || carry; i++) {
            if (i == digits.size())
                digits.push_back(0);
            
            int other_digit = (i < other.digits.size()) ? other.digits[i] : 0;
            int sum = digits[i] + other_digit + carry;
            carry = sum / 10;
            digits[i] = sum % 10;
        }
        return *this;
    }

    BigInt& operator-=(const BigInt& other) {
        if (*this < other)
            throw underflow_error("Result would be negative");
        
        int borrow = 0;
        for (int i = 0; i < digits.size(); i++) {
            int other_digit = (i < other.digits.size()) ? other.digits[i] : 0;
            int diff = digits[i] - borrow - other_digit;
            
            if (diff < 0) {
                diff += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            
            digits[i] = diff;
        }
        
        remove_leading_zeros();
        return *this;
    }

    BigInt& operator*=(const BigInt& other) {
        if (is_zero() || other.is_zero()) {
            *this = BigInt(0);
            return *this;
        }

        vector<int> result(digits.size() + other.digits.size(), 0);

        for (int i = 0; i < digits.size(); i++) {
            for (int j = 0; j < other.digits.size(); j++) {
                result[i+j] += digits[i] * other.digits[j];
                result[i+j+1] += result[i+j] / 10;
                result[i+j] %= 10;
            }
        }

        digits.clear();
        for (int num : result) {
            digits.push_back(num);
        }

        remove_leading_zeros();
        return *this;
    }

    BigInt& operator/=(const BigInt& divisor) {
        if (divisor.is_zero()) {
            throw runtime_error("Division by zero!");
        }
        if (*this < divisor) {
            *this = BigInt(0);
            return *this;
        }
        if (*this == divisor) {
            *this = BigInt(1);
            return *this;
        }

        BigInt quotient;
        BigInt current_dividend;
        
        for (int i = digits.size()-1; i >= 0; i--) {
            current_dividend.digits.insert(current_dividend.digits.begin(), digits[i]);
            current_dividend.remove_leading_zeros();
            
            int count = 0;
            while (current_dividend >= divisor) {
                current_dividend -= divisor;
                count++;
            }
            quotient.digits.push_back(count);
        }
        
        reverse(quotient.digits.begin(), quotient.digits.end());
        quotient.remove_leading_zeros();
        *this = quotient;
        return *this;
    }

    BigInt operator%(const BigInt& divisor) const {
        if (divisor.is_zero()) {
            throw runtime_error("Modulo by zero!");
        }
        
        BigInt remainder = *this;
        BigInt quotient = *this / divisor;
        remainder -= quotient * divisor;
        return remainder;
    }

    BigInt pow(const BigInt& exponent) const {
        if (exponent.is_zero()) return BigInt(1);
        BigInt result(1);
        BigInt counter(0);
        
        while (counter < exponent) {
            result *= *this;
            ++counter;
        }
        return result;
    }

    BigInt sqrt() const {
        if (*this < BigInt(0)) {
            throw invalid_argument("Square root of negative number");
        }
        if (is_zero() || *this == BigInt(1)) return *this;

        BigInt low(1), high = *this;
        BigInt result;
        
        while (low <= high) {
            BigInt mid = (low + high) / BigInt(2);
            BigInt mid_sq = mid * mid;
            
            if (mid_sq == *this) return mid;
            
            if (mid_sq < *this) {
                low = mid + BigInt(1);
                result = mid;
            } else {
                high = mid - BigInt(1);
            }
        }
        return result;
    }

    // Comparison operators
    friend bool operator<(const BigInt& a, const BigInt& b) {
        if (a.digits.size() != b.digits.size())
            return a.digits.size() < b.digits.size();
        
        for (int i = a.digits.size()-1; i >= 0; i--) {
            if (a.digits[i] != b.digits[i])
                return a.digits[i] < b.digits[i];
        }
        return false;
    }

    friend bool operator==(const BigInt& a, const BigInt& b) {
        return a.digits == b.digits;
    }

    friend bool operator!=(const BigInt& a, const BigInt& b) { return !(a == b); }
    friend bool operator>(const BigInt& a, const BigInt& b)  { return b < a; }
    friend bool operator<=(const BigInt& a, const BigInt& b) { return !(a > b); }
    friend bool operator>=(const BigInt& a, const BigInt& b) { return !(a < b); }

    // Input/output
    friend ostream& operator<<(ostream& os, const BigInt& num) {
        for (auto it = num.digits.rbegin(); it != num.digits.rend(); ++it)
            os << (int)*it;
        return os;
    }

    friend istream& operator>>(istream& is, BigInt& num) {
        string s;
        is >> s;
        num = BigInt(s);
        return is;
    }
};

// Non-member operator implementations
BigInt operator+(BigInt a, const BigInt& b) { return a += b; }
BigInt operator-(BigInt a, const BigInt& b) { return a -= b; }
BigInt operator*(BigInt a, const BigInt& b) { return a *= b; }
BigInt operator/(BigInt a, const BigInt& b) { return a /= b; }

// Special mathematical functions
BigInt factorial(int n) {
    if (n < 0) throw invalid_argument("Negative factorial");
    BigInt result(1);
    for (int i = 2; i <= n; ++i) {
        result *= BigInt(i);
    }
    return result;
}

BigInt fibonacci(int n) {
    BigInt a(0), b(1);
    if (n == 0) return a;
    for (int i = 2; i <= n; ++i) {
        BigInt next = a + b;
        a = b;
        b = next;
    }
    return b;
}

BigInt catalan(int n) {
    if (n < 0) throw invalid_argument("Negative Catalan number");
    BigInt num = factorial(2 * n);
    BigInt denom = factorial(n + 1) * factorial(n);
    return num / denom;
}

int main() {
    // Basic operations
    BigInt a("123456789");
    BigInt b("987654321");
    
    cout << "a = " << a << "\n";
    cout << "b = " << b << "\n";
    cout << "a + b = " << a + b << "\n";
    cout << "b - a = " << b - a << "\n";
    cout << "a * b = " << a * b << "\n\n";

    // Division and modulo
    BigInt c("100");
    BigInt d("3");
    cout << "100 / 3 = " << c / d << "\n";
    cout << "100 % 3 = " << c % d << "\n\n";

    // Power and sqrt
    BigInt e("2");
    cout << "2^10 = " << e.pow(BigInt(10)) << "\n";
    BigInt f("1000000");
    cout << "sqrt(1000000) = " << f.sqrt() << "\n\n";

    // Special functions
    cout << "5! = " << factorial(5) << "\n";
    cout << "fib(10) = " << fibonacci(10) << "\n";
    cout << "catalan(4) = " << catalan(4) << "\n";

    return 0;
}