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
#include "ores.reporting.api/domain/cut_approval_detail.hpp"
#include "ores.reporting.api/domain/cut_gap.hpp"
#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <expected>
#include <map>
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
 * @brief The cuts a person is reading: a kind, a month, a text in the label, a
 * state and the page of them.
 *
 * Every narrowing is one the person chose and every one may be empty, in which
 * case it narrows nothing. The page states which rows the read answers and the
 * count states how many it matched, so a person reads that the page is not the
 * whole of it.
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

    /** @brief How many of the rows the query matched to pass over. */
    std::uint32_t offset = 0;

    /** @brief How many rows to answer, up to the read's own paging cap. */
    std::uint32_t limit = 0;
};

/** @brief The rows a query read, and how many it matched. */
struct cut_page {
    std::vector<cut_row_view> rows;
    int total = 0;
};

/**
 * @brief One cut's authorisation request as the request page draws it.
 *
 * The request is the inbox's record of the ask and the decision; what it is
 * about lives in this component's detail row. The subject states the body a
 * person quotes, the occasion, the scope and the counts of the parts the take
 * fixed, and the label and the state are derived here and stored nowhere, so the
 * request page and the cut's own page cannot disagree about the body.
 */
struct cut_authorisation_subject_view {
    boost::uuids::uuid request_id;
    boost::uuids::uuid cut_id;
    std::string label;
    std::string kind_code;
    std::string kind_name;
    std::chrono::system_clock::time_point timepoint;
    std::string timepoint_timezone;
    std::string scope_code;
    std::string business_centre_code;
    cut_state state = cut_state::draft;
    int deal_count = 0;
    int market_data_set_count = 0;
    int gap_count = 0;

    /** @brief The account the apply stamped the cut with, or empty while none has. */
    std::string applied_by;

    /** @brief When the decision was stamped onto the cut, or nothing while it waits. */
    std::optional<std::chrono::system_clock::time_point> applied_at;
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

    /**
     * @brief The subject of the approval request the take raised, or nothing
     * where the kind required none.
     *
     * The counts of the body's parts are the detail row's, so the page that
     * states them reads one record rather than counting the parts again.
     */
    std::optional<domain::cut_approval_detail> detail;
};

/**
 * @brief The instant window a month names, or the whole store's when it names none.
 *
 * The month is a range rather than a year and a month, so the caller's clock
 * decides what a month is and the store answers instants. An empty month reads
 * everything the store holds, and its window ends a day past now because a body
 * may fix a close the day has not reached yet.
 *
 * The month is the caller's input, so a wrong one is a refusal and not an
 * exception: the caller reads the sentence that says which form the field takes
 * and answers it as the bad request it is.
 *
 * @return the window, or the sentence that refuses the month.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::expected<
    std::pair<std::chrono::system_clock::time_point, std::chrono::system_clock::time_point>,
    std::string>
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
     * @return the page, or the sentence that refuses a narrowing the caller
     * stated, such as a month that is not an ISO 8601 year and month.
     */
    [[nodiscard]] std::expected<cut_page, std::string> list(const cut_list_query& query);

    /**
     * @brief Reads one cut as its record page draws it, or nothing when the
     * session may not see it.
     */
    std::optional<cut_record_view> record(const boost::uuids::uuid& cut_id);

    /**
     * @brief Reads the body each cut-authorisation request authorises.
     *
     * One read for a page of requests: the subject lives in this component's
     * detail row, keyed by the request, and each row's cut is read beside it.
     * A request this component holds no detail row for answers nothing, which is
     * what a request raised outside a take leaves behind, so the caller draws the
     * kind alone and says why.
     */
    std::vector<cut_authorisation_subject_view>
    authorisation_subjects(const std::vector<boost::uuids::uuid>& request_ids);

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

    /** @brief The kind of one body, read by the code the body names. */
    std::optional<domain::cut_kind> kind_of(const std::string& code);

    /** @brief How many report runs priced from one body. */
    int run_count(const boost::uuids::uuid& cut_id);

    /**
     * @brief How many report runs priced from each body on the page.
     *
     * One read answers the runs of the whole page, so a list of a hundred cuts
     * costs one read and not a hundred.
     */
    std::map<boost::uuids::uuid, int> run_counts(const std::vector<boost::uuids::uuid>& cut_ids);

    context ctx_;
};

}

#endif
