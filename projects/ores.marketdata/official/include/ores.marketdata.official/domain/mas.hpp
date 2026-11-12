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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_MAS_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_MAS_HPP

#include "ores.http.client/net/transport.hpp"
#include "ores.marketdata.official/domain/fetcher.hpp"
#include "ores.marketdata.official/export.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::official::domain {

/**
 * @brief The hidden fields an ASP.NET page states, which a post back to it must return.
 */
struct mas_page_state final {
    std::string view_state;
    std::string view_state_generator;
    std::string event_validation;
};

/// The hidden fields of the MAS domestic interest rates page @p html.
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::expected<mas_page_state, std::string>
parse_mas_page_state(std::string_view html);

/**
 * @brief The fixings in a download of the MAS domestic interest rates, read as the series
 * @p key.
 *
 * The download is CSV with a title above its header and notes below its rows. The header
 * names =SORA Value Date= over three columns, the year, the month and the day, then =SORA
 * Publication Date= and =SORA=. A row states its year and its month only when they change,
 * so each carries down. The rate is in percent.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_mas_sora_csv(std::string_view csv, std::string_view key);

/**
 * @brief Reads SORA from the domestic interest rates page of the Monetary Authority of
 * Singapore.
 *
 * The only key is =SORA=. MAS's API needs a Singapore Corppass login, so the fetcher does what
 * the page's Download button does: it reads the page for its hidden fields and posts them back
 * with a range of months and the SORA column. The page needs no key and no cookie.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT mas_fetcher final : public fetcher {
public:
    mas_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "mas_dir";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "mas_dir";
    }

    [[nodiscard]] std::string_view field() const override {
        return "SORA";
    }

    /// The key @p key, when it is =SORA=.
    [[nodiscard]] std::expected<std::vector<std::string>, std::string>
    universe(std::string_view key) override;

    /// The fixings of SORA from @p from to @p to, both included.
    [[nodiscard]] std::expected<std::vector<published_fixing>, std::string>
    fixings(std::string_view key,
            std::chrono::year_month_day from,
            std::chrono::year_month_day to) override;

private:
    http::client::net::transport& transport_;
    std::string base_url_;
};

}

#endif
