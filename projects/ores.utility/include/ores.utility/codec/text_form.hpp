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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_CODEC_TEXT_FORM_HPP
#define ORES_UTILITY_CODEC_TEXT_FORM_HPP

#include "ores.utility/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <filesystem>
#include <magic_enum/magic_enum.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>

namespace boost::asio::ip {
class address;
}

namespace ores::utility::decimal {
class decimal;
}

namespace ores::utility::codec {

/**
 * @brief The text a type takes on the wire.
 *
 * The types with a custom encoding whose wire form is a string state the
 * spelling of that string once here, so the JSON and the MessagePack carriers
 * write the same text and read the same text, and so a value that goes out
 * through one comes back through the other. A type whose wire form is not a
 * string states it in its carrier's own specialisation instead -- an error code
 * is the one such type today.
 *
 * A type that has a text form specialises this with three members: =wire_name=,
 * which names it in a refusal, =to_text=, and =from_text=.
 *
 * The forms of the concrete types are declared here and defined in
 * text_form.cpp, because a form of a decimal needs the decimal's definition and
 * every translation unit that encodes a message was parsing it for nothing. An
 * enumerator's form is a template, so it stays here with the rest of the class:
 * it is defined once and instantiated by the codec that meets the enumerator.
 */
template <typename T>
struct text_form;

/**
 * @brief An enumerator is its own name.
 *
 * A value of an enumerated type travels as the enumerator's name: the text is
 * the spelling magic_enum gives it, so a name this build knows reads back as the
 * enumerator and the other way round.
 *
 * A name this build does not know is refused with the text that arrived. An
 * enumerator outside the declared range names no text, and to_text answers the
 * empty string for it, which from_text refuses in turn; a type that gives that
 * value a meaning states its own form, as product_type does.
 */
template <typename T>
    requires std::is_enum_v<T>
struct text_form<T> {
    static constexpr std::string_view wire_name = "an enumerator";

    static std::string to_text(T v) {
        return std::string(magic_enum::enum_name(v));
    }

    static T from_text(std::string_view text) {
        const auto v = magic_enum::enum_cast<T>(text);
        if (!v) {
            std::string names;
            for (const auto name : magic_enum::enum_names<T>()) {
                names += (names.empty() ? "" : ", ");
                names += name;
            }
            throw std::invalid_argument("Not " + std::string(wire_name) + ": '" +
                                        std::string(text) + "' is not one of " + names);
        }
        return *v;
    }
};

/**
 * @brief A string is its own text form.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<std::string> {
    static constexpr std::string_view wire_name = "a string";

    static std::string to_text(const std::string& v);
    static std::string from_text(std::string_view text);
};

/**
 * @brief A filesystem path is its native string.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<std::filesystem::path> {
    static constexpr std::string_view wire_name = "a path";

    static std::string to_text(const std::filesystem::path& v);
    static std::filesystem::path from_text(std::string_view text);
};

/**
 * @brief A tenant id's text form is the uuid it is, spelled the same way.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<ores::utility::uuid::tenant_id> {
    static constexpr std::string_view wire_name = "a tenant id";

    static std::string to_text(const ores::utility::uuid::tenant_id& v);
    static ores::utility::uuid::tenant_id from_text(std::string_view text);
};

/**
 * @brief A date's text form is =YYYY-MM-DD=. The default-constructed date is
 * the absence the domain uses, and its text form is the empty string, which
 * reads back as that same value.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<std::chrono::year_month_day> {
    static constexpr std::string_view wire_name = "a date";

    static std::string to_text(const std::chrono::year_month_day& v);
    static std::chrono::year_month_day from_text(std::string_view text);
};

/**
 * @brief A decimal's text form is its own exact spelling, never a number a
 * reader would round through a binary float.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<ores::utility::decimal::decimal> {
    static constexpr std::string_view wire_name = "a decimal";

    static std::string to_text(const ores::utility::decimal::decimal& v);
    static ores::utility::decimal::decimal from_text(std::string_view text);
};

/**
 * @brief An address's text form is the address's own spelling.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<boost::asio::ip::address> {
    static constexpr std::string_view wire_name = "an address";

    static std::string to_text(const boost::asio::ip::address& v);
    static boost::asio::ip::address from_text(std::string_view text);
};

/**
 * @brief An instant's text form is the wire shape the time architecture
 * states, six digits of microseconds and a UTC designator.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<std::chrono::system_clock::time_point> {
    static constexpr std::string_view wire_name = "an instant";

    static std::string to_text(const std::chrono::system_clock::time_point& v);
    static std::chrono::system_clock::time_point from_text(std::string_view text);
};

/**
 * @brief A uuid's text form is the canonical lower-case spelling, with the
 * dashes, which is what every other layer of the estate writes.
 */
template <>
struct ORES_UTILITY_EXPORT text_form<boost::uuids::uuid> {
    static constexpr std::string_view wire_name = "a uuid";

    static std::string to_text(const boost::uuids::uuid& v);
    static boost::uuids::uuid from_text(std::string_view text);
};

}

#endif
