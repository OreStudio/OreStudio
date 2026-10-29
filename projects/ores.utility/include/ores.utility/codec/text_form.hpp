/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_UTILITY_CODEC_TEXT_FORM_HPP
#define ORES_UTILITY_CODEC_TEXT_FORM_HPP

#include "ores.platform/time/datetime.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::utility::codec {

/**
 * @brief The text a type takes on the wire.
 *
 * Every type with a custom encoding is a string on the wire, and the custom
 * part is the spelling of that string. The spelling is stated once here, so the
 * JSON and the MessagePack carriers write the same text and read the same text,
 * and so a value that goes out through one comes back through the other.
 *
 * A type that has a text form specialises this with three members: =wire_name=,
 * which names it in a refusal, =to_text=, and =from_text=.
 */
template <typename T>
struct text_form;

/**
 * @brief A string is its own text form.
 */
template <>
struct text_form<std::string> {
    static constexpr std::string_view wire_name = "a string";

    static std::string to_text(const std::string& v) {
        return v;
    }

    static std::string from_text(std::string_view text) {
        return std::string(text);
    }
};

/**
 * @brief A tenant id's text form is the uuid it is, spelled the same way.
 */
template <>
struct text_form<ores::utility::uuid::tenant_id> {
    static constexpr std::string_view wire_name = "a tenant id";

    static std::string to_text(const ores::utility::uuid::tenant_id& v) {
        return v.to_string();
    }

    static ores::utility::uuid::tenant_id from_text(std::string_view text) {
        auto result = ores::utility::uuid::tenant_id::from_string(std::string(text));
        if (!result)
            throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text) +
                                        ": " + result.error());
        return *result;
    }
};

/**
 * @brief A date's text form is =YYYY-MM-DD=. The default-constructed date is
 * the absence the domain uses, and its text form is the empty string, which
 * reads back as that same value.
 */
template <>
struct text_form<std::chrono::year_month_day> {
    static constexpr std::string_view wire_name = "a date";

    static std::string to_text(const std::chrono::year_month_day& v) {
        return v.ok() ? std::format("{:%Y-%m-%d}", v) : std::string{};
    }

    static std::chrono::year_month_day from_text(std::string_view text) {
        if (text.empty())
            return {};
        try {
            return ores::platform::time::datetime::from_iso8601_date(std::string(text));
        } catch (const std::exception& e) {
            throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text) +
                                        ": " + e.what());
        }
    }
};

/**
 * @brief A decimal's text form is its own exact spelling, never a number a
 * reader would round through a binary float.
 */
template <>
struct text_form<ores::utility::decimal::decimal> {
    static constexpr std::string_view wire_name = "a decimal";

    static std::string to_text(const ores::utility::decimal::decimal& v) {
        return v.to_string();
    }

    static ores::utility::decimal::decimal from_text(std::string_view text) {
        auto result = ores::utility::decimal::decimal::from_string(std::string(text));
        if (!result)
            throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text) +
                                        ": " + result.error());
        return *result;
    }
};

/**
 * @brief An address's text form is the address's own spelling.
 */
template <>
struct text_form<boost::asio::ip::address> {
    static constexpr std::string_view wire_name = "an address";

    static std::string to_text(const boost::asio::ip::address& v) {
        return v.to_string();
    }

    static boost::asio::ip::address from_text(std::string_view text) {
        try {
            return boost::asio::ip::make_address(std::string(text));
        } catch (const std::exception& e) {
            throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text) +
                                        ": " + e.what());
        }
    }
};

/**
 * @brief An instant's text form is the wire shape the time architecture
 * states, six digits of microseconds and a UTC designator.
 */
template <>
struct text_form<std::chrono::system_clock::time_point> {
    static constexpr std::string_view wire_name = "an instant";

    static std::string to_text(const std::chrono::system_clock::time_point& v) {
        return ores::platform::time::datetime::to_iso8601_utc(v);
    }

    static std::chrono::system_clock::time_point from_text(std::string_view text) {
        try {
            return ores::platform::time::datetime::from_iso8601_utc(std::string(text));
        } catch (const std::exception& e) {
            throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text) +
                                        ": " + e.what());
        }
    }
};

/**
 * @brief A uuid's text form is the canonical lower-case spelling, with the
 * dashes, which is what every other layer of the estate writes.
 */
template <>
struct text_form<boost::uuids::uuid> {
    static constexpr std::string_view wire_name = "a uuid";

    static std::string to_text(const boost::uuids::uuid& v) {
        return boost::uuids::to_string(v);
    }

    static boost::uuids::uuid from_text(std::string_view text) {
        try {
            return boost::lexical_cast<boost::uuids::uuid>(text);
        } catch (const boost::bad_lexical_cast& e) {
            throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text) +
                                        ": " + e.what());
        }
    }
};

}

#endif
