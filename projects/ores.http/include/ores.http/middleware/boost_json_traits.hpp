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
#ifndef ORES_HTTP_MIDDLEWARE_BOOST_JSON_TRAITS_HPP
#define ORES_HTTP_MIDDLEWARE_BOOST_JSON_TRAITS_HPP

#include <boost/json.hpp>

namespace jwt::traits {

/**
 * @brief jwt-cpp traits for boost::json.
 *
 * Provides the type mappings and operations required by jwt-cpp
 * to use boost::json as its JSON backend.
 */
struct boost_json {
    using value_type = boost::json::value;
    using object_type = boost::json::object;
    using array_type = boost::json::array;
    using string_type = std::string;
    using number_type = double;
    using integer_type = std::int64_t;
    using boolean_type = bool;

    static value_type parse(const std::string& str) {
        return boost::json::parse(str);
    }

    static std::string serialize(const value_type& val) {
        return boost::json::serialize(val);
    }

    static bool is_object(const value_type& val) {
        return val.is_object();
    }

    static bool is_array(const value_type& val) {
        return val.is_array();
    }

    static bool is_string(const value_type& val) {
        return val.is_string();
    }

    static bool is_number(const value_type& val) {
        return val.is_double() || val.is_int64() || val.is_uint64();
    }

    static bool is_integer(const value_type& val) {
        return val.is_int64() || val.is_uint64();
    }

    static bool is_bool(const value_type& val) {
        return val.is_bool();
    }

    static bool is_null(const value_type& val) {
        return val.is_null();
    }

    static object_type as_object(const value_type& val) {
        return val.as_object();
    }

    static array_type as_array(const value_type& val) {
        return val.as_array();
    }

    static string_type as_string(const value_type& val) {
        return std::string(val.as_string());
    }

    static number_type as_number(const value_type& val) {
        if (val.is_double()) {
            return val.as_double();
        } else if (val.is_int64()) {
            return static_cast<double>(val.as_int64());
        } else {
            return static_cast<double>(val.as_uint64());
        }
    }

    static integer_type as_integer(const value_type& val) {
        if (val.is_int64()) {
            return val.as_int64();
        } else {
            return static_cast<integer_type>(val.as_uint64());
        }
    }

    static boolean_type as_bool(const value_type& val) {
        return val.as_bool();
    }

    static bool parse_object_element(object_type::iterator it,
        std::string& name, value_type& val) {
        name = std::string(it->key());
        val = it->value();
        return true;
    }

    static bool parse_array_element(array_type::const_iterator it,
        value_type& val) {
        val = *it;
        return true;
    }
};

}

#endif
