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
#include "ores.utility/codec/text_form.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <filesystem>
#include <format>
#include <stdexcept>
#include <string>

namespace ores::utility::codec {

namespace {

[[noreturn]] void refuse(std::string_view what, std::string_view text, std::string_view why) {
    throw std::invalid_argument("Not " + std::string(what) + ": " + std::string(text) + ": " +
                                std::string(why));
}

}

std::string text_form<std::string>::to_text(const std::string& v) {
    return v;
}

std::string text_form<std::string>::from_text(std::string_view text) {
    return std::string(text);
}

std::string text_form<std::filesystem::path>::to_text(const std::filesystem::path& v) {
    return v.string();
}

std::filesystem::path text_form<std::filesystem::path>::from_text(std::string_view text) {
    return std::filesystem::path(std::string(text));
}

std::string
text_form<ores::utility::uuid::tenant_id>::to_text(const ores::utility::uuid::tenant_id& v) {
    return v.to_string();
}

ores::utility::uuid::tenant_id
text_form<ores::utility::uuid::tenant_id>::from_text(std::string_view text) {
    auto result = ores::utility::uuid::tenant_id::from_string(std::string(text));
    if (!result)
        refuse(wire_name, text, result.error());
    return *result;
}

std::string text_form<std::chrono::year_month_day>::to_text(const std::chrono::year_month_day& v) {
    return v.ok() ? std::format("{:%Y-%m-%d}", v) : std::string{};
}

std::chrono::year_month_day
text_form<std::chrono::year_month_day>::from_text(std::string_view text) {
    if (text.empty())
        return {};
    try {
        return ores::platform::time::datetime::from_iso8601_date(std::string(text));
    } catch (const std::exception& e) {
        refuse(wire_name, text, e.what());
    }
}

std::string
text_form<ores::utility::decimal::decimal>::to_text(const ores::utility::decimal::decimal& v) {
    return v.to_string();
}

ores::utility::decimal::decimal
text_form<ores::utility::decimal::decimal>::from_text(std::string_view text) {
    auto result = ores::utility::decimal::decimal::from_string(std::string(text));
    if (!result)
        refuse(wire_name, text, result.error());
    return *result;
}

std::string text_form<boost::asio::ip::address>::to_text(const boost::asio::ip::address& v) {
    return v.to_string();
}

boost::asio::ip::address text_form<boost::asio::ip::address>::from_text(std::string_view text) {
    try {
        return boost::asio::ip::make_address(std::string(text));
    } catch (const std::exception& e) {
        refuse(wire_name, text, e.what());
    }
}

std::string text_form<std::chrono::system_clock::time_point>::to_text(
    const std::chrono::system_clock::time_point& v) {
    return ores::platform::time::datetime::to_iso8601_utc(v);
}

std::chrono::system_clock::time_point
text_form<std::chrono::system_clock::time_point>::from_text(std::string_view text) {
    try {
        return ores::platform::time::datetime::from_iso8601_utc(std::string(text));
    } catch (const std::exception& e) {
        refuse(wire_name, text, e.what());
    }
}

std::string text_form<boost::uuids::uuid>::to_text(const boost::uuids::uuid& v) {
    return boost::uuids::to_string(v);
}

boost::uuids::uuid text_form<boost::uuids::uuid>::from_text(std::string_view text) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(text);
    } catch (const boost::bad_lexical_cast& e) {
        refuse(wire_name, text, e.what());
    }
}

}
