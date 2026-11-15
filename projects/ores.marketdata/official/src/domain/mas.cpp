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
#include "ores.marketdata.official/domain/mas.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <charconv>
#include <format>
#include <optional>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

constexpr std::string_view sora_key = "SORA";
constexpr std::string_view page_path = "/statistics/dir/DomesticInterestRates.aspx";
constexpr std::string_view field_prefix = "ctl00$ContentPlaceHolder1$";
/// The position of SORA in the page's list of columns.
constexpr std::string_view sora_column = "ColumnsCheckBoxList$13";

/// The value of the hidden input named @p name in @p html.
std::optional<std::string> hidden_value(std::string_view html, std::string_view name) {
    const auto at = html.find(std::format("name=\"{}\"", name));
    if (at == std::string_view::npos)
        return std::nullopt;
    const auto tag_end = html.find('>', at);
    const auto value = html.find("value=\"", at);
    if (value == std::string_view::npos || value > tag_end)
        return std::nullopt;
    const auto begin = value + 7;
    const auto end = html.find('"', begin);
    if (end == std::string_view::npos)
        return std::nullopt;
    return std::string(html.substr(begin, end - begin));
}

/// @p text percent-encoded for a form body, every byte but the unreserved ones.
std::string form_encoded(std::string_view text) {
    std::string out;
    for (const unsigned char c : text) {
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') ||
            c == '-' || c == '.' || c == '_' || c == '~')
            out.push_back(static_cast<char>(c));
        else
            out += std::format("%{:02X}", c);
    }
    return out;
}

std::optional<std::chrono::month> sora_month_of(std::string_view name) {
    for (unsigned m = 1; m <= 12; ++m)
        if (month_name(std::chrono::month{m}) == name)
            return std::chrono::month{m};
    return std::nullopt;
}

std::optional<int> number_of(std::string_view text) {
    int out = 0;
    const auto* end = text.data() + text.size();
    const auto [ptr, ec] = std::from_chars(text.data(), end, out);
    if (ec != std::errc{} || ptr != end || text.empty())
        return std::nullopt;
    return out;
}

}

std::expected<mas_page_state, std::string> parse_mas_page_state(std::string_view html) {
    auto view_state = hidden_value(html, "__VIEWSTATE");
    auto generator = hidden_value(html, "__VIEWSTATEGENERATOR");
    auto validation = hidden_value(html, "__EVENTVALIDATION");
    if (!view_state || !generator || !validation)
        return std::unexpected(
            "the page is not the MAS domestic interest rates form: it lacks a hidden field");
    return mas_page_state{std::move(*view_state), std::move(*generator), std::move(*validation)};
}

std::expected<std::vector<published_fixing>, std::string> parse_mas_sora_csv(std::string_view csv,
                                                                             std::string_view key) {
    const auto lines = lines_of(csv);
    const auto header = std::ranges::find_if(
        lines, [](std::string_view line) { return line.starts_with("SORA Value Date"); });
    if (header == lines.end())
        return std::unexpected("the answer is not a SORA download: it has no SORA Value Date");
    const auto names = cells_of(*header, ',');
    if (names.size() < 5 || names[4] != "SORA")
        return std::unexpected("the download's fifth column is not SORA");

    std::vector<published_fixing> out;
    std::optional<int> year;
    std::optional<std::chrono::month> month;
    for (auto line = std::next(header); line != lines.end(); ++line) {
        if (line->starts_with('"'))
            break;
        // A download that spans years states the header again above each year.
        if (line->starts_with("SORA Value Date"))
            continue;
        const auto cells = cells_of(*line, ',');
        if (cells.size() < 5)
            return std::unexpected(std::format("a SORA row has {} cells: {}", cells.size(), *line));
        if (!cells[0].empty())
            year = number_of(cells[0]);
        if (!cells[1].empty())
            month = sora_month_of(cells[1]);
        const auto day = number_of(cells[2]);
        if (!year || !month || !day)
            return std::unexpected(std::format("a SORA row names no whole date: {}", *line));
        const std::chrono::year_month_day date{
            std::chrono::year{*year}, *month, std::chrono::day{static_cast<unsigned>(*day)}};
        if (!date.ok())
            return std::unexpected(std::format("a SORA row names no real date: {}", *line));
        if (cells[4].empty())
            continue;
        auto value = decimal_of_percent(cells[4]);
        if (!value)
            return std::unexpected(value.error());
        out.push_back({date, std::string(key), std::move(*value)});
    }
    return out;
}

mas_fetcher::mas_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> mas_fetcher::universe(std::string_view key) {
    if (key != sora_key)
        return std::unexpected("'" + std::string(key) +
                               "' is not a column of MAS's domestic interest rates the fetcher "
                               "reads: the only one is SORA");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> mas_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    const auto url = base_url_ + std::string(page_path);
    auto page = read_body(transport_, url);
    if (!page)
        return std::unexpected(page.error());
    auto state = parse_mas_page_state(*page);
    if (!state)
        return std::unexpected(state.error());

    const auto field = [](std::string_view name, std::string_view value) {
        return form_encoded(name) + "=" + form_encoded(value);
    };
    const auto named = [](std::string_view name) {
        return std::string(field_prefix) + std::string(name);
    };
    const std::string body =
        field("__VIEWSTATE", state->view_state) + "&" +
        field("__VIEWSTATEGENERATOR", state->view_state_generator) + "&" +
        field("__EVENTVALIDATION", state->event_validation) + "&" +
        field(named("StartYearDropDownList"), std::to_string(static_cast<int>(from.year()))) + "&" +
        field(named("EndYearDropDownList"), std::to_string(static_cast<int>(to.year()))) + "&" +
        field(named("StartMonthDropDownList"),
              std::to_string(static_cast<unsigned>(from.month()))) +
        "&" +
        field(named("EndMonthDropDownList"), std::to_string(static_cast<unsigned>(to.month()))) +
        "&" + field(named(sora_column), "on") + "&" + field(named("Button2"), "Download");

    auto answer = transport_.post(url, "application/x-www-form-urlencoded", body);
    if (!answer)
        return std::unexpected(answer.error().message);
    if (answer->status != 200)
        return std::unexpected(std::format("{} answered {} to the download", url, answer->status));
    auto read = parse_mas_sora_csv(answer->body, key);
    if (!read)
        return std::unexpected(read.error());
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
