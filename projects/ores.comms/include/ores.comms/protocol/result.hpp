/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_COMMS_PROTOCOL_RESULT_HPP
#define ORES_COMMS_PROTOCOL_RESULT_HPP

#include <optional>
#include <variant>

namespace ores::comms::protocol {

/**
 * @brief Simple result type for C++20 (std::expected replacement).
 *
 * Holds either a value of type T or an error of type E.
 */
template<typename T, typename E>
class result final {
public:
    result(const T& value) : data_(value) {}
    result(T&& value) : data_(std::move(value)) {}
    result(const E& error) : data_(error) {}
    result(E&& error) : data_(std::move(error)) {}

    bool has_value() const { return std::holds_alternative<T>(data_); }
    bool has_error() const { return std::holds_alternative<E>(data_); }

    explicit operator bool() const { return has_value(); }

    const T& value() const & { return std::get<T>(data_); }
    T& value() & { return std::get<T>(data_); }
    T&& value() && { return std::get<T>(std::move(data_)); }

    const T& operator*() const & { return value(); }
    T& operator*() & { return value(); }
    T&& operator*() && { return std::move(*this).value(); }

    const T* operator->() const { return &value(); }
    T* operator->() { return &value(); }

    const E& error() const & { return std::get<E>(data_); }
    E& error() & { return std::get<E>(data_); }
    E&& error() && { return std::get<E>(std::move(data_)); }

private:
    std::variant<T, E> data_;
};

/**
 * @brief Specialization for void value type.
 */
template<typename E>
class result<void, E> final {
public:
    result() : error_(std::nullopt) {}
    result(const E& error) : error_(error) {}
    result(E&& error) : error_(std::move(error)) {}

    bool has_value() const { return !error_.has_value(); }
    bool has_error() const { return error_.has_value(); }

    explicit operator bool() const { return has_value(); }

    const E& error() const & { return *error_; }
    E& error() & { return *error_; }
    E&& error() && { return std::move(*error_); }

private:
    std::optional<E> error_;
};

}

#endif
