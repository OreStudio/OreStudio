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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_READ_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_READ_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/cut.hpp"
#include "ores.reporting.api/domain/cut_gap.hpp"
#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief One row of the cuts list: a body, what it declares and what derived from it.
 *
 * The state, the label and the run tally are derived here and never stored, so
 * the list and the record read one derivation rather than two.
 */
struct cut_row_view {
    domain::cut cut;

    /**
     * @brief The kind the body was taken under.
     *
     * Empty when the store holds no kind of that code, which is what a retired
     * kind leaves behind: the row then states no label and no kind name rather
     * than inventing either.
     */
    std::optional<domain::cut_kind> kind;

    /** @brief The label a person quotes for the body, or empty without its kind. */
    std::string label;

    cut_state state = cut_state::draft;

    /** @brief The later body that holds this one's place, where one does. */
    std::optional<boost::uuids::uuid> superseded_by;

    /** @brief How many report runs priced from the body. */
    int run_count = 0;
};

/**
 * @brief The cuts a person is reading: a kind, a month, a text in the label and a state.
 *
 * Every member is a narrowing the person chose and every one may be empty, in
 * which case it narrows nothing.
 */
struct cut_list_query {
    /** @brief The kind to read, or empty for every kind. */
    std::string kind_code;

    /** @brief The month of the instants, as @c YYYY-MM, or empty for every month. */
    std::string month;

    /** @brief A text the label must contain, or empty for every row. */
    std::string search;

    /** @brief A derived state to keep, or empty for every state. */
    std::string state;
};

/** @brief The rows a query read, and how many it matched. */
struct cut_page {
    std::vector<cut_row_view> rows;
    int total = 0;
};

/** @brief One cut as its record page reads it. */
struct cut_record_view {
    domain::cut cut;
    std::optional<domain::cut_kind> kind;
    std::string label;
    cut_state state = cut_state::draft;
    std::optional<boost::uuids::uuid> superseded_by;
    std::vector<domain::cut_gap> gaps;
    int run_count = 0;
};

/**
 * @brief The instant window a month names, or the whole store's when it names none.
 *
 * The month is a range rather than a year and a month, so the caller's clock
 * decides what a month is and the store answers instants. An empty month reads
 * everything the store holds, and its window ends a day past now because a body
 * may fix a close the day has not reached yet.
 *
 * @throws std::invalid_argument if the month is not an ISO 8601 year and month.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT
std::pair<std::chrono::system_clock::time_point, std::chrono::system_clock::time_point>
cut_window_of(const std::string& month);

/**
 * @brief The earliest later body a run may price from, where one holds this one's place.
 *
 * The body that replaced a cut is the first of the later bodies of the same
 * kind, scope and centre that a run may price from: every later body in turn
 * replaced that one, so the one a person is pointed at is the earliest.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::optional<boost::uuids::uuid>
replacement_of(const domain::cut& cut, const std::vector<domain::cut>& candidates);

/**
 * @brief Reads the cuts a person reads, with the state and the label derived.
 *
 * The state and the label are functions of the facts a cut records, and the
 * design keeps them out of the store so that a row and its state cannot
 * disagree. The derivation lives here for the same reason: a screen that
 * derived either would be a second statement of the rule, and the list, the
 * record and the resolution would drift apart.
 *
 * The reads are the party's, which is the session's: a cut belongs to the party
 * whose books it fixed, and the store refuses a row the session may not see.
 */
class ORES_REPORTING_CORE_EXPORT cut_read_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.cut_read_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit cut_read_service(context ctx);

    /**
     * @brief Reads the cuts a query names, newest instant first.
     *
     * @throws std::invalid_argument if the month is not an ISO 8601 year and
     * month, so a caller hears which field it stated wrongly.
     */
    cut_page list(const cut_list_query& query);

    /**
     * @brief Reads one cut as its record page draws it, or nothing when the
     * session may not see it.
     */
    std::optional<cut_record_view> record(const boost::uuids::uuid& cut_id);

private:
    /**
     * @brief Derives each row's state, its supersession and the body that replaced it.
     *
     * Supersession is a function of every later body of the same kind, scope and
     * centre, so the candidates are read past the window the person is reading:
     * a body fixed last month is superseded by the one fixed this morning.
     */
    void derive_rows(std::vector<cut_row_view>& rows,
                     const std::vector<domain::cut>& candidates,
                     const std::vector<domain::cut_gap>& gaps);

    /** @brief The kinds the store holds, by code, for the labels and the names. */
    std::vector<domain::cut_kind> kinds();

    /** @brief How many report runs priced from one body. */
    int run_count(const boost::uuids::uuid& cut_id);

    context ctx_;
};

}

#endif
