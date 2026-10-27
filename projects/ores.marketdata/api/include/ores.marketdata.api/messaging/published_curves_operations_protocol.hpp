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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_MESSAGING_PUBLISHED_CURVES_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PUBLISHED_CURVES_OPERATIONS_PROTOCOL_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One pillar of a published set.
 *
 * The pillar is the coordinate the datum stands at -- the term of an interest
 * rate curve -- and the value is the text the publish wrote, exact. The kind
 * says what the value denotes, so a reader that shows a rate and the discount
 * factor it implies knows which of the two it was given.
 */
struct published_curve_point {
    std::string pillar;
    std::string value;
    std::string value_kind;
    std::string oresmd_uri;
    std::chrono::system_clock::time_point observation_datetime;
};

/**
 * @brief One published set, or the candidate that waits to become one.
 *
 * The state is a function of the record's own facts and is stated here rather
 * than stored: `withdrawn` when the set was taken out of use, `superseded` when
 * a later set replaced it, `published` when a person applied a request and the
 * set is in use, and `candidate` when the request still waits. A candidate
 * holds no publisher and no published instant, so a reader cannot mistake it
 * for the set a desk prices from.
 *
 * A set read from the list carries its point count and no points; a set read
 * for one curve carries its points, because the screen draws them.
 */
struct published_curve_set {
    std::string publish_id;
    std::string request_id;
    std::string curve;
    std::string series_id;
    std::string series_uri;
    std::string bootstrap_config_id;
    std::string party;
    std::string party_id;
    std::chrono::system_clock::time_point as_of;
    std::string published_by;
    std::optional<std::chrono::system_clock::time_point> published_at;
    std::string state_code;
    std::uint32_t point_count = 0;
    std::string withdrawn_by;
    std::optional<std::chrono::system_clock::time_point> withdrawn_at;
    std::string withdrawn_reason_code;
    std::string withdrawal_request_id;
    std::optional<std::chrono::system_clock::time_point> superseded_at;
    std::string superseded_by_request_id;
    std::optional<std::chrono::system_clock::time_point> superseded_as_of;
    std::vector<published_curve_point> points;
};

/**
 * @brief The published sets of the tenant, newest first, and the candidates.
 *
 * A reader sees the sets in use by default. A candidate is a set nobody has
 * approved and an ended set is one that was in use and is not any more, so
 * neither is drawn unless the caller asks for it: a candidate read as the set
 * a desk prices from would be a lie, and an ended set is reached on purpose.
 *
 * The total is the size of the whole filtered set and not of the page, so a
 * screen can page without a second read.
 */
struct list_published_curves_request {
    using response_type = struct list_published_curves_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.list_published_curves";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id = "";
    std::string search = "";
    bool include_candidates = false;
    bool include_ended = false;
    std::vector<std::string> request_id_one_of;
    std::uint32_t offset = 0;
    std::uint32_t limit = 50;
};

/**
 * @brief The rows of the published curve list.
 *
 * The order is the screen's and not the caller's: the sets in use come first,
 * newest published instant first, and the candidates follow in as-of order, so
 * a set nobody approved never stands above a set a desk prices from.
 */
struct list_published_curves_response {
    std::vector<published_curve_set> sets;
    std::uint32_t total = 0;
    bool success = false;
    std::string message;
};

/**
 * @brief The last sets published for one series, newest last, with their points.
 *
 * The sets come back oldest first, so a reader draws them in the order they
 * happened and colours the newest one strongest. A candidate of the same curve
 * is included when it is asked for, with no published instant, so the
 * comparison tab reads the baseline and the candidate from one answer.
 */
struct get_published_curve_sets_request {
    using response_type = struct get_published_curve_sets_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_published_curve_sets";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string series_id;
    std::uint32_t limit = 5;
    bool include_candidates = true;
};

/**
 * @brief One curve's sets, oldest first, each with the points it holds.
 *
 * An ended set is returned like any other published set, because it was
 * published; its state says that it is not the set in use.
 */
struct get_published_curve_sets_response {
    std::vector<published_curve_set> sets;
    std::uint32_t published_count = 0;
    bool success = false;
    std::string message;
};

/**
 * @brief Takes a published set out of use, as a new version of its record.
 *
 * The set is not deleted and its values are not cleared: the record keeps the
 * publisher, the published instant and the set's points, and gains the account
 * that ended it, the instant and the reason. A set that already ended cannot be
 * ended again, and a set that was never published cannot be withdrawn, because
 * a candidate is not in use.
 *
 * A withdrawal names no request when the tenant lets a trader act alone, and
 * names one when a decision stands behind the act. A request authorises one
 * withdrawal, so a request that already ended a set ends nothing else.
 */
struct withdraw_curve_request {
    using response_type = struct withdraw_curve_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.withdraw_curve";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string publish_id;
    std::string reason_code;
    std::string comment = "";
    std::string approval_request_id = "";
};

/**
 * @brief The instant and the account the record now holds.
 *
 * The answer states the withdrawal the record carries so a screen can show the
 * ended set without reading it again.
 */
struct withdraw_curve_response {
    std::chrono::system_clock::time_point withdrawn_at;
    std::string withdrawn_by;
    bool success = false;
    std::string message;
};

}

#endif
