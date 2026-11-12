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
#include "ores.marketdata.official/domain/bcch.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/parse_options.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

/// @p text with every byte outside the unreserved set of RFC 3986 percent-encoded.
std::string query_encoded(std::string_view text) {
    std::string out;
    for (const unsigned char c : text) {
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') ||
            c == '-' || c == '.' || c == '_' || c == '~')
            out += static_cast<char>(c);
        else
            out += std::format("%{:02X}", c);
    }
    return out;
}

}

std::expected<std::vector<published_fixing>, std::string> parse_bcch_series(std::string_view json,
                                                                            std::string_view key) {
    boost::json::parse_options options;
    options.allow_invalid_utf8 = true;
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec, {}, options);
    if (ec)
        return std::unexpected("the series is not JSON: " + ec.message());
    const auto* root = doc.if_object();
    const auto* code = root == nullptr ? nullptr : root->if_contains("Codigo");
    if (code == nullptr || !code->is_int64())
        return std::unexpected("the answer has no Codigo");
    if (code->as_int64() != 0) {
        const auto* why = root->if_contains("Descripcion");
        return std::unexpected(std::format(
            "the service answered code {}: {}",
            code->as_int64(),
            why != nullptr && why->is_string() ? std::string(why->as_string()) : "no reason"));
    }
    const auto* series = root->if_contains("Series");
    const auto* id = series == nullptr || !series->is_object() ?
                         nullptr :
                         series->as_object().if_contains("seriesId");
    if (id == nullptr || !id->is_string() || id->as_string() != key)
        return std::unexpected("the answer is not of the series " + std::string(key));
    const auto* observations = series->as_object().if_contains("Obs");
    if (observations == nullptr || !observations->is_array())
        return std::unexpected("the Obs of the series is not an array");
    std::vector<published_fixing> out;
    for (const auto& item : observations->as_array()) {
        const auto* entry = item.if_object();
        const auto* date = entry == nullptr ? nullptr : entry->if_contains("indexDateString");
        const auto* value = entry == nullptr ? nullptr : entry->if_contains("value");
        const auto* status = entry == nullptr ? nullptr : entry->if_contains("statusCode");
        if (date == nullptr || !date->is_string() || value == nullptr || !value->is_string() ||
            status == nullptr || !status->is_string())
            return std::unexpected(
                "an observation of the series has no text indexDateString, value and statusCode");
        if (status->as_string() != "OK")
            continue;
        auto parsed = date_of_day_first(date->as_string(), '-');
        if (!parsed)
            return std::unexpected(parsed.error());
        auto decimal = decimal_of_percent(value->as_string());
        if (!decimal)
            return std::unexpected(decimal.error());
        out.push_back({*parsed, std::string(key), std::move(*decimal)});
    }
    return out;
}

bcch_fetcher::bcch_fetcher(http::client::net::transport& transport,
                           std::string base_url,
                           std::expected<bcch_account, std::string> account)
    : transport_(transport)
    , base_url_(std::move(base_url))
    , account_(std::move(account)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> bcch_fetcher::universe(std::string_view key) {
    if (key.empty() || key.size() > 64 || !std::ranges::all_of(key, [](char c) {
            return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '.' || c == '_';
        }))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series id: it holds only capitals, digits, dots and "
                               "underscores, such as F022.TIB.TIP.D001.NO.Z.D");
    if (!account_)
        return std::unexpected("the Banco Central de Chile needs an account: " + account_.error());
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> bcch_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    auto body =
        read_body(transport_,
                  std::format("{}/SieteRestWS/SieteRestWS.ashx?user={}&pass={}&function=GetSeries"
                              "&timeseries={}&firstdate={}&lastdate={}",
                              base_url_,
                              query_encoded(account_->user),
                              query_encoded(account_->password),
                              key,
                              iso(from),
                              iso(to)));
    if (!body)
        return std::unexpected(body.error());
    auto read = parse_bcch_series(*body, key);
    if (!read)
        return std::unexpected(read.error());
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
