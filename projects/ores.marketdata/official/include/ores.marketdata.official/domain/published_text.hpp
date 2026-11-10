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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_PUBLISHED_TEXT_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_PUBLISHED_TEXT_HPP

#include "ores.http.client/net/transport.hpp"
#include "ores.marketdata.official/export.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>

namespace ores::marketdata::official::domain {

/// The shortest text that reads back to @p value.
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::string text_of(double value);

/// The date of an ISO text such as =2026-10-29=.
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::chrono::year_month_day, std::string>
    date_of(std::string_view text);

/**
 * @brief The decimal of a percent published as @p text, such as =0.0397= for =3.97=.
 *
 * The decimal point moves two places in the text, so every published digit is kept and no
 * binary rounding enters. The rates of the system are decimals.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::expected<std::string, std::string>
decimal_of_percent(std::string_view text);

/// The ISO text of @p date, such as =2026-10-29=.
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::string iso(std::chrono::year_month_day date);

/// The body of a GET of @p url, or why there is none. An answer other than 200 is an error.
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::expected<std::string, std::string>
read_body(http::client::net::transport& transport, const std::string& url);

}

#endif
