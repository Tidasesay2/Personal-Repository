// Remember to add the header comments, and comments for each
// of the functions and methods you implement
// Edited by: Tidankay Sesay
// Date: April 26th, 2023

#include "expression.h"
#include <sstream>
#include <stack>
using std::stack;

/**
 * Got this from: https://stackoverflow.com/questions/1798112/removing-leading-and-trailing-spaces-from-a-string
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * W A R N I N G        W A R N I N G        W A R N I N G        W A R N I N G    * *
 * Even when I am giving you code that I found on the Internet, you are still      * *
 * not allowed to copy ANY code from the Internet or from any AI tool.             * *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * @param str the string that we want to trim from trailing and leading spaces
 * @param whitespace the whitespaces we want to remove, by default it will remove space and tab
 * @return a string without leading or trailing spaces.
 */
string trim(const string& str, const string& whitespace){
    const auto strBegin = str.find_first_not_of(whitespace);
    if (strBegin == string::npos)
        return ""; // no content
    const auto strEnd = str.find_last_not_of(whitespace);
    const auto strRange = strEnd - strBegin + 1;
    return str.substr(strBegin, strRange);
}
Expression::Expression() { // Default constructor empty strings
    _infix = "";
    _postfix = "";
}

Expression::Expression(const string &infix) { //constructed and initialized with  infix string input
    _infix = infix;
    _postfix = ""; // for an empty tree
}
bool isOperator(char Char) { // checking of character is an operator
    return (Char == '+' || Char == '-' || Char== '*' || Char == '/'); // Returns true if the character is one of the operators, and false otherwise.
}
void Expression::ConvertToPostfix() { // converts the expression to postfix
    stack<char> opStack;
    std::stringstream output;
    unsigned long _position = 0;


    while (_position < _infix.length()) { // goes through the infix
        string element = Next(_infix, _position);
        if (!element.empty() && (isalpha(element[0]) || isdigit(element[0]))) { // will add element if its operand
            output << element << " ";
        }
        else if (element == "(") { // pushes to opStack if it's in the ()
            opStack.push(element[0]);
        }

        else if (element == ")") { // statement loops and pops till matching parenthesis is found (right parenthesis)
            while (opStack.top() != '(') {
                output << opStack.top() << " ";
                opStack.pop();
            }
            opStack.pop();
        }
        else if (isOperator(element[0])) { //element = op , then pops until smaller op is found, will then push back to the stack

            while (!opStack.empty() && opStack.top() != '(' && Precedence(opStack.top(), element[0])) {
                output << opStack.top() << " ";
                opStack.pop();
            }
            opStack.push(element[0]);
        }
    }
    // Pops/add leftovers to postfix
    while (!opStack.empty()) {
        output << opStack.top() << " ";
        opStack.pop();
    }
    _postfix = output.str(); //setting
    _postfix.pop_back();
}

string Expression::GetInfix()const{ // will return infix string
    return _infix;
}

string Expression::GetPostfix()const{ // will return postfix string
    return _postfix;
}

string Expression::ToJSON() const{ //returning both as json
    return R"({"infix":")" + _infix + R"(", "postfix":")" + _postfix + R"("})";
}
double Expression::Evaluate(bool &error) const{
    error = false; //sets to false
    stack<double> evStack; // stores vals

    std::istringstream iss(_postfix);//checks whether each element in the string _postfix is a number
    string _element;

    while (iss >> _element){ // iterates through its characters and setting a boolean variable
        bool is_number = true;

            for (char _Char : _element) {
                if (!isdigit(_Char)) {
                 is_number = false;
                break;
            }
        }
        if (is_number){
            evStack.push(stod(_element)); //pushes number to stack
        }else{
            if(evStack.size() < 2) {
                error = true; //sets errors
                    return 0;
            }
            double _op2 = evStack.top(); //popping from stack
            evStack.pop();
            double op1 = evStack.top();
            

            evStack.pop();
            if (_element == "+") {
                evStack.push(op1 + _op2);

            } else if (_element == "-") {
                evStack.push(op1 - _op2);

            } else if (_element == "*") {
                evStack.push(op1 * _op2);

            } else if (_element == "/") {
                if (_op2 == 0) { //if dividing by zero error
                    error = true;
                        return 0;
                }
                evStack.push(op1 / _op2);
            } else {
                error = true;
                return 0;
            }
        }
    }
    if(evStack.size() != 1){ //checks whether the size of the stack is equal to one, and if not sets the error
        error = true;
            return 0;
    }
    return evStack.top();
}
bool Expression::Precedence(char op1, char op2) { // precedence between two operators
    if (op1 == '*' || op1 == '/') {
        return true;

    } else if (op1 == '+' || op1 == '-') {

        return op2 != '*' && op2 != '/';
    } else {
        return false;
    }
}
string Expression::Next(const string &str, unsigned long &pos) { //returns string based off parameters
    string next;
    if(pos >= str.length()) {
         return next;
    }

    if(isdigit(str[pos]) || str[pos] == '.') { //checking for digits or decimals
        while (pos < str.length() && (isdigit(str[pos]) || str[pos] == '.')) {
            next += str[pos];
            pos++;
        }

    }else if (isalpha(str[pos])) { //checks for alphabets
        while (pos < str.length() && (isalpha(str[pos]) || isdigit(str[pos]))) {
            next += str[pos];
            pos++;
        }
    } else {
        next += str[pos];
        pos++;
    }
    return next;
}



