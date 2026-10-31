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
#include "ores.reporting.core/service/cut_read_service.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/repository/cut_gap_repository.hpp"
#include "ores.reporting.core/repository/cut_kind_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include <algorithm>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cctype>
#include <chrono>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::reporting::service {

using namespace ores::logging;

namespace {

/**
 * @brief How many rows a list answers.
 *
 * A list reads the newest page of a scope that grows at the rate of its
 * occasion: the count states how many the query matched, so a person reads that
 * the page is not the whole of it.
 */
constexpr int max_rows = 1000;

}

std::expected<std::pair<std::chrono::system_clock::time_point,
                        std::chrono::system_clock::time_point>,
              std::string>
cut_window_of(const std::string& month) {
    if (month.empty()) {
        const auto from = std::chrono::system_clock::time_point(
            std::chrono::sys_days(std::chrono::year{1970} / std::chrono::January / 1));
        const auto to = std::chrono::system_clock::now() + std::chrono::hours(24);
        return std::make_pair(from, to);
    }

    const auto refusal = [&month] {
        return std::unexpected("A month is stated as YYYY-MM, and this one is '" + month + "'.");
    };
    if (month.size() != 7 || month[4] != '-')
        return refusal();
    const auto digits = [&month](std::size_t offset, std::size_t length) {
        return std::all_of(month.begin() + static_cast<std::ptrdiff_t>(offset),
                           month.begin() + static_cast<std::ptrdiff_t>(offset + length),
                           [](char c) { return c >= '0' && c <= '9'; });
    };
    if (!digits(0, 4) || !digits(5, 2))
        return refusal();

    const std::chrono::year_month start{
        std::chrono::year{std::stoi(month.substr(0, 4))},
        std::chrono::month{static_cast<unsigned>(std::stoi(month.substr(5, 2)))}};
    if (!start.ok())
        return refusal();

    const auto after = start + std::chrono::months{1};
    return std::make_pair(
        std::chrono::system_clock::time_point(
            std::chrono::sys_days(start.year() / start.month() / 1)),
        std::chrono::system_clock::time_point(
            std::chrono::sys_days(after.year() / after.month() / 1)));
}

namespace {

/** @brief Whether a state's name is the one a filter states. */
bool names(const cut_state state, const std::string& name) {
    return name.empty() || to_string(state) == name;
}

/** @brief A text the label must contain, read without regard to case. */
std::string lowered(std::string value) {
    std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return value;
}

}

cut_read_service::cut_read_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::cut_kind> cut_read_service::kinds() {
    repository::cut_kind_repository kinds_repo;
    return kinds_repo.read_latest(ctx_);
}

int cut_read_service::run_count(const boost::uuids::uuid& cut_id) {
    repository::report_instance_repository runs_repo;
    messaging::report_instances_filter filter;
    filter.cut_id = cut_id;
    return static_cast<int>(runs_repo.get_total_instance_count(ctx_, filter));
}

void cut_read_service::derive_rows(std::vector<cut_row_view>& rows,
                                   const std::vector<domain::cut>& candidates,
                                   const std::vector<domain::cut_gap>& gaps) {
    std::map<boost::uuids::uuid, int> open_gaps;
    for (const auto& gap : gaps)
        ++open_gaps[gap.cut_id];

    for (auto& row : rows) {
        const auto open = open_gaps.find(row.cut.id);
        const auto gap_count = open == open_gaps.end() ? 0 : open->second;
        const auto replaced = later_cut_holds_its_place(row.cut, candidates);
        row.state = derive_state(row.cut, gap_count, replaced);
        if (replaced)
            row.superseded_by = replacement_of(row.cut, candidates);
    }
}

std::optional<boost::uuids::uuid>
replacement_of(const domain::cut& cut, const std::vector<domain::cut>& candidates) {
    std::optional<boost::uuids::uuid> replacement;
    auto replacement_timepoint = std::chrono::system_clock::time_point::max();
    for (const auto& other : candidates) {
        if (other.id == cut.id || other.kind_code != cut.kind_code ||
            other.party_id != cut.party_id || other.scope_code != cut.scope_code ||
            other.business_centre_code != cut.business_centre_code ||
            other.timepoint <= cut.timepoint || !is_usable(derive_state(other, 0)))
            continue;
        if (replacement && other.timepoint >= replacement_timepoint)
            continue;
        replacement = other.id;
        replacement_timepoint = other.timepoint;
    }
    return replacement;
}

std::expected<cut_page, std::string> cut_read_service::list(const cut_list_query& query) {
    const auto party_id = ctx_.party_id();
    if (!party_id) {
        BOOST_LOG_SEV(lg(), warn) << "A cuts list was read with no party in the session.";
        return cut_page{};
    }

    const auto window_of_month = cut_window_of(query.month);
    if (!window_of_month) {
        BOOST_LOG_SEV(lg(), warn) << "A cuts list was refused: " << window_of_month.error();
        return std::unexpected(window_of_month.error());
    }
    const auto [from, to] = *window_of_month;
    repository::cut_repository cuts_repo;
    auto window = cuts_repo.read_for_party_in_window(ctx_, *party_id, query.kind_code, from, to);

    // Supersession is a function of every later body of the same kind, scope and
    // centre, and a body fixed at the end of one month is replaced by one fixed
    // in the next. The candidates are therefore read past the window, from the
    // window's end to a day past now, and the window's own rows are added to
    // them so a body replaces the one before it inside the month too.
    std::vector<domain::cut> candidates = window;
    if (to < std::chrono::system_clock::now()) {
        auto later = cuts_repo.read_for_party_in_window(
            ctx_,
            *party_id,
            query.kind_code,
            to,
            std::chrono::system_clock::now() + std::chrono::hours(24));
        candidates.insert(candidates.end(), later.begin(), later.end());
    }

    std::vector<cut_row_view> rows;
    rows.reserve(window.size());
    for (const auto& cut : window)
        rows.push_back(cut_row_view{.cut = cut});

    repository::cut_gap_repository gaps_repo;
    derive_rows(rows, candidates, gaps_repo.read_latest(ctx_));

    // A label is derived from the kind as well as from the cut, so the kinds are
    // read once for the page rather than once for each row.
    const auto all_kinds = kinds();
    std::map<std::string, domain::cut_kind> kinds_by_code;
    for (const auto& kind : all_kinds)
        kinds_by_code[kind.code] = kind;

    for (auto& row : rows) {
        const auto found = kinds_by_code.find(row.cut.kind_code);
        if (found == kinds_by_code.end()) {
            BOOST_LOG_SEV(lg(), warn) << "Cut " << boost::uuids::to_string(row.cut.id)
                                      << " names the kind " << row.cut.kind_code
                                      << ", which the store does not hold.";
            continue;
        }
        row.kind = found->second;
        row.label = cut_label_of(row.cut, found->second);
    }

    const auto search = lowered(query.search);
    auto matching = std::vector<cut_row_view>();
    for (auto& row : rows) {
        if (!names(row.state, query.state))
            continue;
        if (!search.empty() && lowered(row.label).find(search) == std::string::npos)
            continue;
        row.run_count = run_count(row.cut.id);
        matching.push_back(std::move(row));
    }

    cut_page page;
    page.total = static_cast<int>(matching.size());
    if (matching.size() > max_rows)
        matching.resize(max_rows);
    page.rows = std::move(matching);
    return page;
}

std::optional<cut_record_view> cut_read_service::record(const boost::uuids::uuid& cut_id) {
    repository::cut_repository cuts_repo;
    auto found = cuts_repo.read_latest(ctx_, boost::uuids::to_string(cut_id));
    if (found.empty())
        return std::nullopt;

    const auto& cut = found.front();

    // The candidates that could hold this body's place are the later ones of the
    // same kind, scope and centre, so the read covers a year past the body: a
    // superseded cut stays in the list it was read from, and a body fixed years
    // ago is still replaced by the one fixed since.
    auto candidates = cuts_repo.read_for_party_in_window(
        ctx_,
        cut.party_id,
        cut.kind_code,
        cut.timepoint - std::chrono::hours(24),
        std::chrono::system_clock::now() + std::chrono::hours(24));

    cut_record_view record;
    record.cut = cut;

    repository::cut_gap_repository gaps_repo;
    record.gaps = gaps_repo.read_latest_by_cut_id(ctx_, boost::uuids::to_string(cut_id), 0, 1000);

    std::vector<cut_row_view> rows{cut_row_view{.cut = cut}};
    derive_rows(rows, candidates, record.gaps);
    record.state = rows.front().state;
    record.superseded_by = rows.front().superseded_by;

    for (const auto& kind : kinds()) {
        if (kind.code != cut.kind_code)
            continue;
        record.kind = kind;
        record.label = cut_label_of(cut, kind);
        break;
    }
    if (!record.kind)
        BOOST_LOG_SEV(lg(), warn) << "Cut " << boost::uuids::to_string(cut.id)
                                  << " names the kind " << cut.kind_code
                                  << ", which the store does not hold.";

    record.run_count = run_count(cut.id);
    return record;
}

}
