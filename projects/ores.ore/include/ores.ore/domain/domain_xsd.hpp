
#pragma once

#ifndef XSDCPP_H
#define XSDCPP_H

#include <cstdint>
#include <string>
#include <vector>

namespace xsd {

typedef std::string string;

namespace detail {

// Custom bool vector using char storage to avoid std::vector<bool>'s proxy type.
class bool_vector : public std::vector<char>
{
public:
    using std::vector<char>::vector;
    void push_back(bool value) { std::vector<char>::push_back(value ? 1 : 0); }
    void emplace_back() { std::vector<char>::emplace_back(0); }
    bool& back() { return reinterpret_cast<bool&>(std::vector<char>::back()); }
    const bool& back() const { return reinterpret_cast<const bool&>(std::vector<char>::back()); }
};

template <typename T>
struct vector_selector { using type = std::vector<T>; };

template <>
struct vector_selector<bool> { using type = bool_vector; };

} // namespace detail

// vector<T> is std::vector<T> for all non-bool types (preserving MSVC's lenient
// handling of incomplete element types in generated headers), and the char-backed
// bool_vector for bool (avoiding std::vector<bool>'s bit-packing proxy).
template <typename T>
using vector = typename detail::vector_selector<T>::type;

template <typename T>
class optional
{
public:
    optional()
        : _data(nullptr)
    {
    }
    optional(const optional& other)
        : _data(other._data ? new T(*other._data) : nullptr)
    {
    }

    optional(optional&& other)
    {
        _data = other._data;
        other._data = nullptr;
    }

    optional(const T& other)
        : _data(new T(other))
    {
    }

    optional(T&& other)
        : _data(new T(std::move(other)))
    {
    }

    ~optional()
    {
        delete _data;
    }

    optional& operator=(const optional& other)
    {
        if (other._data)
        {
            if (_data)
                *_data = *other._data;
            else
                _data = new T(*other._data);
        }
        else
        {
            delete _data;
            _data = nullptr;
        }
        return *this;
    }

    optional& operator=(optional&& other)
    {
        if (_data)
            delete _data;
        _data = other._data;
        other._data = nullptr;
        return *this;
    }

    optional& operator=(const T& other)
    {
        if (_data)
            *_data = other;
        else
            _data = new T(other);
        return *this;
    }

    optional& operator=(T&& other)
    {
        if (_data)
            *_data = std::move(other);
        else
            _data = new T(std::move(other));
        return *this;
    }

    operator bool() const { return _data != nullptr; }

    T& operator*() { return *_data; }
    const T& operator*() const { return *_data; }
    T* operator->() { return _data; }
    const T* operator->() const { return _data; }

    friend bool operator==(const optional& lh, const T& rh) { return lh._data && *lh._data == rh; }
    friend bool operator!=(const optional& lh, const T& rh) { return !lh._data || *lh._data != rh; }
    friend bool operator==(const T& lh, const optional& rh) { return rh._data && lh == rh._data; }
    friend bool operator!=(const T& lh, const optional& rh) { return !rh._data || lh != rh._data; }
    friend bool operator==(const optional& lh, const optional& rh) { return lh._data == rh._data || (lh._data && rh._data && *lh._data == *rh._data); }
    friend bool operator!=(const optional& lh, const optional& rh) { return lh._data != rh._data && (!lh._data || !rh._data || *lh._data != *rh._data); }

private:
    T* _data;
};

template <typename T>
class base
{
public:
    base()
        : _value()
    {
    }
    base(T value) 
        : _value(value)
    {
    }

    operator T() const { return _value; }
    operator T&() { return _value; }

    base& operator=(T value)
    {
        _value = value;
        return *this;
    }

private:
    T _value;
};

struct any_attribute
{
    std::string name;
    xsd::string value;
};

struct any_element
{
    std::string name;
    xsd::string value;
};

}

#endif
