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
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
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
 * @brief How many rows a read answers, whatever the caller asks for.
 *
 * A cut accumulates at the rate of its occasion and a LIVE cut at the rate of
 * the runs, so a read is bounded and the count states how many the query
 * matched. The bound is the read's own and not the caller's: a caller narrows
 * the page it reads and never widens the read.
 */
constexpr std::uint32_t paging_cap = 1000;

/** @brief The page a query asks for, bounded by the read's own cap. */
std::pair<std::size_t, std::size_t> page_bounds(const cut_list_query& query, std::size_t matched) {
    const auto offset = std::min<std::size_t>(query.offset, matched);
    const auto asked = query.limit == 0 ? paging_cap : std::min(query.limit, paging_cap);
    return {offset, std::min<std::size_t>(asked, matched - offset)};
}

}

std::expected<
    std::pair<std::chrono::system_clock::time_point, std::chrono::system_clock::time_point>,
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
    return std::make_pair(std::chrono::system_clock::time_point(
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

std::optional<domain::cut_kind> cut_read_service::kind_of(const std::string& code) {
    repository::cut_kind_repository kinds_repo;
    const auto found = kinds_repo.read_latest(ctx_, code);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

int cut_read_service::run_count(const boost::uuids::uuid& cut_id) {
    repository::report_instance_repository runs_repo;
    messaging::report_instances_filter filter;
    filter.cut_id = cut_id;
    return static_cast<int>(runs_repo.get_total_instance_count(ctx_, filter));
}

std::map<boost::uuids::uuid, int>
cut_read_service::run_counts(const std::vector<boost::uuids::uuid>& cut_ids) {
    std::map<boost::uuids::uuid, int> counts;
    if (cut_ids.empty())
        return counts;

    // The runs of the whole page are one read: a run names the cut it priced
    // from, so the page's tally is a count of the rows the read answered.
    repository::report_instance_repository runs_repo;
    messaging::report_instances_filter filter;
    filter.cut_id_one_of = cut_ids;
    const auto runs = runs_repo.read_latest(ctx_, 0, paging_cap, {}, filter);
    for (const auto& run : runs) {
        if (run.cut_id)
            ++counts[*run.cut_id];
    }
    return counts;
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

std::optional<boost::uuids::uuid> replacement_of(const domain::cut& cut,
                                                 const std::vector<domain::cut>& candidates) {
    std::optional<boost::uuids::uuid> replacement;
    auto replacement_timepoint = std::chrono::system_clock::time_point::max();
    for (const auto& other : candidates) {
        if (!stands_in_the_place_of(other, cut))
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
        auto later = cuts_repo.read_for_party_in_window(ctx_,
                                                        *party_id,
                                                        query.kind_code,
                                                        to,
                                                        std::chrono::system_clock::now() +
                                                            std::chrono::hours(24));
        candidates.insert(candidates.end(), later.begin(), later.end());
    }

    std::vector<cut_row_view> rows;
    rows.reserve(window.size());
    for (const auto& cut : window)
        rows.push_back(cut_row_view{.cut = cut});

    repository::cut_gap_repository gaps_repo;
    derive_rows(rows, candidates, gaps_repo.read_latest(ctx_));

    // A label is derived from the kind as well as from the cut, so each kind a
    // row names is read once for the page and no kind the page does not name is
    // read at all.
    std::map<std::string, domain::cut_kind> kinds_by_code;
    for (auto& row : rows) {
        auto found = kinds_by_code.find(row.cut.kind_code);
        if (found == kinds_by_code.end()) {
            const auto kind = kind_of(row.cut.kind_code);
            if (!kind) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Cut " << boost::uuids::to_string(row.cut.id) << " names the kind "
                    << row.cut.kind_code << ", which the store does not hold.";
                continue;
            }
            found = kinds_by_code.emplace(kind->code, *kind).first;
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
        matching.push_back(std::move(row));
    }

    const auto [offset, answered] = page_bounds(query, matching.size());
    cut_page page;
    page.total = static_cast<int>(matching.size());

    // The page's tally is one read of the runs of every body on it, so the list
    // costs one read rather than one for each row.
    std::vector<boost::uuids::uuid> page_ids;
    page_ids.reserve(answered);
    for (auto at = offset; at < offset + answered; ++at)
        page_ids.push_back(matching[at].cut.id);
    const auto counts = run_counts(page_ids);

    page.rows.reserve(answered);
    for (auto at = offset; at < offset + answered; ++at) {
        auto row = std::move(matching[at]);
        const auto tally = counts.find(row.cut.id);
        row.run_count = tally == counts.end() ? 0 : tally->second;
        page.rows.push_back(std::move(row));
    }
    return page;
}

std::optional<cut_record_view> cut_read_service::record(const boost::uuids::uuid& cut_id) {
    repository::cut_repository cuts_repo;
    auto found = cuts_repo.read_latest(ctx_, boost::uuids::to_string(cut_id));
    if (found.empty())
        return std::nullopt;

    const auto& cut = found.front();

    // The candidates that could hold this body's place are the later ones of the
    // same kind, scope and centre. The read starts a day before the body, so the
    // body itself is in it, and ends a day past now, so a body fixed years ago is
    // still replaced by the one fixed since.
    auto candidates = cuts_repo.read_for_party_in_window(ctx_,
                                                         cut.party_id,
                                                         cut.kind_code,
                                                         cut.timepoint - std::chrono::hours(24),
                                                         std::chrono::system_clock::now() +
                                                             std::chrono::hours(24));

    cut_record_view record;
    record.cut = cut;

    repository::cut_gap_repository gaps_repo;
    record.gaps =
        gaps_repo.read_latest_by_cut_id(ctx_, boost::uuids::to_string(cut_id), 0, paging_cap);

    std::vector<cut_row_view> rows{cut_row_view{.cut = cut}};
    derive_rows(rows, candidates, record.gaps);
    record.state = rows.front().state;
    record.superseded_by = rows.front().superseded_by;

    record.kind = kind_of(cut.kind_code);
    if (record.kind)
        record.label = cut_label_of(cut, *record.kind);
    else
        BOOST_LOG_SEV(lg(), warn) << "Cut " << boost::uuids::to_string(cut.id) << " names the kind "
                                  << cut.kind_code << ", which the store does not hold.";

    record.run_count = run_count(cut.id);
    return record;
}

}
