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
#include <jwt-cpp/jwt.h>

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

    static jwt::json::type get_type(const value_type& val) {
        using jwt::json::type;
        if (val.is_bool()) return type::boolean;
        if (val.is_int64() || val.is_uint64()) return type::integer;
        if (val.is_double()) return type::number;
        if (val.is_string()) return type::string;
        if (val.is_array()) return type::array;
        // Default to object for null and other types
        return type::object;
    }

    static bool parse(value_type& val, const string_type& str) {
        try {
            val = boost::json::parse(str);
            return true;
        } catch (...) {
            return false;
        }
    }

    static std::string serialize(const value_type& val) {
        return boost::json::serialize(val);
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

    static boolean_type as_boolean(const value_type& val) {
        return val.as_bool();
    }
};

}

#endif
