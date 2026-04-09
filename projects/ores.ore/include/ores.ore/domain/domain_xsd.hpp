#pragma once

#ifndef XSDCPP_H
#define XSDCPP_H

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <string>
#include <vector>
#include <initializer_list>

namespace xsd {

typedef std::string string;

// vector<T> wraps std::vector<T> for all non-bool types.
//
// The internal std::vector<T> is held through a pointer rather than by value
// or inheritance.  This prevents Clang/GCC from eagerly instantiating
// std::vector<T> when xsd::vector<T> appears as a struct member with an
// incomplete T (a pattern that arises throughout the generated domain.hpp).
// The pointer itself is valid even when T is an incomplete type; the wrapped
// std::vector<T> is only instantiated lazily, at the point where individual
// methods are called — by which time T is always fully defined.
//
// For bool: char storage avoids std::vector<bool>'s bit-packing proxy type.
template <typename T>
class vector {
    std::vector<T>* impl_;

public:
    // ---- Typedefs (match std::vector interface) ----------------------------
    // All use T directly (not via std::vector<T>) to avoid instantiating
    // std::vector<T> with potentially incomplete T at class-definition time.
    using value_type             = T;
    using size_type              = std::size_t;
    using difference_type        = std::ptrdiff_t;
    using reference              = T&;
    using const_reference        = const T&;
    using pointer                = T*;
    using const_pointer          = const T*;
    using iterator               = T*;
    using const_iterator         = const T*;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    // ---- Construction / destruction ----------------------------------------
    vector() : impl_(new std::vector<T>()) {}

    vector(std::initializer_list<T> il) : impl_(new std::vector<T>(il)) {}

    vector(const vector& o) : impl_(new std::vector<T>(*o.impl_)) {}

    vector(vector&& o) noexcept : impl_(o.impl_) { o.impl_ = nullptr; }

    // Defined out-of-class so that the body (delete impl_) is only instantiated
    // at ODR-use, not at the point where xsd::vector<T> is first seen.
    ~vector() noexcept;

    // ---- Assignment --------------------------------------------------------
    vector& operator=(const vector& o) {
        if (this != &o) { delete impl_; impl_ = new std::vector<T>(*o.impl_); }
        return *this;
    }

    vector& operator=(vector&& o) noexcept {
        delete impl_; impl_ = o.impl_; o.impl_ = nullptr; return *this;
    }

    // ---- Size / capacity ---------------------------------------------------
    std::size_t size() const noexcept { return impl_->size(); }
    bool        empty() const noexcept { return impl_->empty(); }
    void        reserve(std::size_t n) { impl_->reserve(n); }
    void        resize(std::size_t n)  { impl_->resize(n); }
    void        clear() noexcept       { impl_->clear(); }

    // ---- Element access ----------------------------------------------------
    T&       operator[](std::size_t n)       { return (*impl_)[n]; }
    const T& operator[](std::size_t n) const { return (*impl_)[n]; }
    T&       at(std::size_t n)               { return impl_->at(n); }
    const T& at(std::size_t n) const         { return impl_->at(n); }
    T&       front()                         { return impl_->front(); }
    const T& front() const                   { return impl_->front(); }
    T&       back()                          { return impl_->back(); }
    const T& back() const                    { return impl_->back(); }
    T*       data()                          { return impl_->data(); }
    const T* data() const                    { return impl_->data(); }

    // ---- Modifiers ---------------------------------------------------------
    void push_back(const T& v)  { impl_->push_back(v); }
    void push_back(T&& v)       { impl_->push_back(std::move(v)); }

    template <typename... Args>
    T& emplace_back(Args&&... args) {
        return impl_->emplace_back(std::forward<Args>(args)...);
    }

    // ---- Iterators ---------------------------------------------------------
    iterator       begin()        { return impl_->data(); }
    const_iterator begin()  const { return impl_->data(); }
    iterator       end()          { return impl_->data() + impl_->size(); }
    const_iterator end()    const { return impl_->data() + impl_->size(); }
    const_iterator cbegin() const { return impl_->data(); }
    const_iterator cend()   const { return impl_->data() + impl_->size(); }

    reverse_iterator       rbegin()        { return reverse_iterator(end()); }
    const_reverse_iterator rbegin()  const { return const_reverse_iterator(end()); }
    reverse_iterator       rend()          { return reverse_iterator(begin()); }
    const_reverse_iterator rend()    const { return const_reverse_iterator(begin()); }

    // ---- Comparison --------------------------------------------------------
    friend bool operator==(const vector& a, const vector& b) {
        if (!a.impl_ || !b.impl_) return a.impl_ == b.impl_;
        return *a.impl_ == *b.impl_;
    }
    friend bool operator!=(const vector& a, const vector& b) { return !(a == b); }
};

// Out-of-class destructor definition: T is only required to be complete at
// the call site (where objects are actually destroyed), not at the point where
// xsd::vector<T> is used as a struct member with forward-declared T.
template <typename T>
inline vector<T>::~vector() noexcept { delete impl_; }

// Specialisation for bool: char storage avoids std::vector<bool>'s proxy type.
template <>
class vector<bool> : public std::vector<char> {
public:
    using std::vector<char>::vector;
    using std::vector<char>::operator=;
    void push_back(bool value) { std::vector<char>::push_back(value ? 1 : 0); }
    void emplace_back() { std::vector<char>::emplace_back(0); }
    bool& back() { return reinterpret_cast<bool&>(std::vector<char>::back()); }
    const bool& back() const { return reinterpret_cast<const bool&>(std::vector<char>::back()); }
};

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
