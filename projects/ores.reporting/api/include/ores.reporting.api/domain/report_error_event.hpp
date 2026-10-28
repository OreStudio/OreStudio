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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_ERROR_EVENT_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_ERROR_EVENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief An error a report run produced, and what it is about.
 *
 * +#+updated: 2026-10-17
 *
 * One error a report run produced, held against the run. A report run is one
 * execution of a report definition, and a run whose analytics fail is still a run
 * that completed: the engine finished, wrote its report, and said in its own log
 * that a trade would not build. Those errors are facts about the run, they belong
 * to it, and they are what this table holds.
 *
 * An error is a fact about an instant: it is written once and never changed. The
 * model therefore states :current_state: and :immutable: in the * SQL **
 * Flags drawer, as [[id:47E6903C-DFA7-4D2F-B044-C1C74428F719][ores.reporting.report_trigger_event]]
 * does. A current-state immutable table has no validity window and no audit tail:
 * there is no version, no valid_from/valid_to, and no modified_by,
 * performed_by, change_reason_code or change_commentary. Because it has no
 * audit tail, the time is a column of its own, event_time.
 *
 * event_time is the moment the errors were collected from the run's output. The
 * canonical rows do state the engine's own time, as the text the engine wrote, and
 * the service's log row keeps it; this column does not hold it. The recorder writes
 * the collection instant because it is an instant the service can state about its
 * own act, and storing the engine's text as a timestamp here would put two
 * different facts in one column. A story that needs the engine's instant adds a
 * column for it.
 *
 * A row says four things: how bad the error is, what it is about, what it said,
 * and which subject it names. The first is severity. The second is scope_code,
 * drawn from the engine's own vocabulary, and it is what places the error:
 * Trade on the trade, Analytics on the analytic, Curve on the curve, and
 * Report on the run itself. The third is message, the engine's own words.
 *
 * The fourth is subject and subject_id, a discriminator and a value, holding
 * the identifier of the thing the error names: trade and a trade id,
 * analytic and an analytic type, curve and a curve id. It is a discriminator
 * and a value rather than one column because the ids are of different kinds — a
 * trade id is a UUID the portfolio builder minted, a curve id is a free-text name
 * such as [IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)] — and one column holding
 * both would be a text column no reader can key on. It is not a database foreign
 * key for the same reason: the engine names trades by its own ids and curves by
 * their ORE names, and neither is a key any table here holds. The pair is stored
 * so that a screen can find every error about one subject, and it is a
 * discriminator rather than a fixed column per kind because the engine adds
 * sub-fields to the groups it already has, as the corpus shows it doing when an
 * Analytics error grew a tradeId.
 *
 * scope_code and subject are not the same question. The scope is the group the
 * engine emitted, which is what the error is about at the level the engine named
 * it; the subject is the particular thing, and it can be narrower than the scope.
 * An Analytics error that names a tradeId is about that trade and not about
 * the analytic as a whole, and its subject says so.
 *
 * A run that fails before the engine sees it, because a workflow step did not
 * complete, produces a row too, with scope Report. That is why the table is one
 * table rather than two: a reader opening a report wants its errors in one place.
 */
struct report_error_event final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this error row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The run the error belongs to. It is a column and not part of the key: a run's errors
     * are its own rows, and the id of an error is what addresses one.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief Party that owns this error (inherited from the run it belongs to).
     */
    boost::uuids::uuid party_id;

    /**
     * @brief What the error is about, as the engine grouped it. Refers to
     * ores_reporting_report_error_scopes_tbl by its code. The column is scope_code rather than
     * scope because scope is a reserved query parameter of the generated list read, and a
     * filterable member may not take its name.
     *
     * A synthetic row states the one scope that needs no subject, because a subject cannot be
     * synthesized: the run a trade id belongs to is not a thing a fixture can invent.
     */
    std::string scope_code = "Report";

    /**
     * @brief How bad the error is: error or warning. The engine states this per message as its
     * category, and an ALERT the engine wrote is an error here. A warning is an error the engine
     * chose to continue past, which is why it is the same table: the repair is the reader's to
     * judge, and hiding it would be the mistake that produced this task.
     *
     * A synthetic row is a warning rather than an error: the severity is a closed set the store
     * checks, and a fixture that drew one at random would be refused.
     */
    std::string severity = "error";

    /**
     * @brief The engine's own words for the error, unaltered.
     */
    std::string message;

    /**
     * @brief What kind of thing subject_id names: trade, analytic, curve or configuration. Absent
     * when the error is about the run as a whole, which the database enforces: an absent subject is
     * admitted only under the Report scope.
     */
    std::optional<std::string> subject;

    /**
     * @brief Identifier of the subject, in the kind subject states. Absent with subject.
     */
    std::optional<std::string> subject_id;

    /**
     * @brief The version of the subject the run read, when the subject is versioned. A trade is one
     * of many rows under one id, so a link to the trade that caused an error names the version as
     * well as the id, and resolves to the row the run saw rather than to whatever the trade is now.
     */
    std::optional<int> subject_version;

    /**
     * @brief The instant the subject was current at, when the subject is versioned. It reads beside
     * subject_version: the version alone names a row, and the instant says when that row was the
     * live one.
     */
    std::optional<std::chrono::system_clock::time_point> subject_as_of;

    /**
     * @brief What the error is about, said in a person's words rather than the engine's: the curve
     * that would not build, or the curve a failed trade needed. It is worked out once, where the
     * engine's log is read, because the engine states a trade's curve only inside the message it
     * writes. A screen that repeated that reading would be a second implementation of the engine's
     * format, and the shell would have none at all.
     */
    std::optional<std::string> cause;

    /**
     * @brief The failure's own fields for the message, as a JSON object, when the node stated any:
     * the exception that says which failure this is, and the kind of warning when it was one. The
     * message alone does not carry them, and they are what tells two errors of one scope apart, so
     * they are kept rather than dropped.
     */
    std::string detail;

    /**
     * @brief When the errors were collected from the run's output. There is no engine instant to
     * copy: the structured log states no time, so this is the moment of collection, written by the
     * recorder.
     */
    std::chrono::system_clock::time_point event_time;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const report_error_event&, const report_error_event&) = default;
};

/**
 * @brief Dispatch-key identifier for report_error_event, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_error_event&) {
    return "ores.reporting.report_error_event";
}

}

#endif
