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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_TRADE_MARKET_GAP_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_TRADE_MARKET_GAP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.reporting.api/domain/report_trade_market_gap.hpp"
#include "ores.reporting.api/messaging/report_trade_market_gap_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_trade_market_gap_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing trade gaps.
 *
 * Provides a higher-level interface for trade gap operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_trade_market_gap_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_trade_market_gap_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_trade_market_gap_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_trade_market_gap_service(const context& ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    ::ores::reporting::messaging::list_report_trade_market_gaps_response
    list_report_trade_market_gaps(
        const ::ores::reporting::messaging::list_report_trade_market_gaps_request& request);
    ::ores::reporting::messaging::get_report_trade_market_gap_response get_report_trade_market_gap(
        const ::ores::reporting::messaging::get_report_trade_market_gap_request& request);
    ::ores::reporting::messaging::get_many_report_trade_market_gaps_response
    get_many_report_trade_market_gaps(
        const ::ores::reporting::messaging::get_many_report_trade_market_gaps_request& request);
    ::ores::reporting::messaging::put_report_trade_market_gap_response put_report_trade_market_gap(
        const ::ores::reporting::messaging::put_report_trade_market_gap_request& request);
    ::ores::reporting::messaging::put_many_report_trade_market_gaps_response
    put_many_report_trade_market_gaps(
        const ::ores::reporting::messaging::put_many_report_trade_market_gaps_request& request);
    ::ores::reporting::messaging::delete_report_trade_market_gap_response
    delete_report_trade_market_gap(
        const ::ores::reporting::messaging::delete_report_trade_market_gap_request& request);
    ::ores::reporting::messaging::delete_many_report_trade_market_gaps_response
    delete_many_report_trade_market_gaps(
        const ::ores::reporting::messaging::delete_many_report_trade_market_gaps_request& request);
    ::ores::reporting::messaging::list_report_trade_market_gap_versions_response
    list_report_trade_market_gap_versions(
        const ::ores::reporting::messaging::list_report_trade_market_gap_versions_request& request);
    ::ores::reporting::messaging::get_report_trade_market_gap_version_response
    get_report_trade_market_gap_version(
        const ::ores::reporting::messaging::get_report_trade_market_gap_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade gaps with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade gaps for the requested page.
     */
    std::vector<domain::report_trade_market_gap> list_trade_gaps(std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade gaps.
     *
     * @return Total number of active trade gaps.
     */
    std::uint32_t count_trade_gaps();


    /**
     * @brief Retrieves a single trade gap as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade gap at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_trade_market_gap>
    get_trade_gap_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single trade gap by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade gap if found, std::nullopt otherwise.
     */
    std::optional<domain::report_trade_market_gap> get_trade_gap(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade gaps by primary key.
     */
    std::vector<domain::report_trade_market_gap>
    get_trade_gaps(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade gap (creates or updates).
     *
     * @param trade_gap The trade gap to save.
     * @throws std::exception on failure.
     */
    void save_trade_gap(const domain::report_trade_market_gap& trade_gap);

    /**
     * @brief Saves a batch of trade gaps.
     *
     * @param trade_gaps The trade gaps to save.
     * @throws std::exception on failure.
     */
    void save_trade_gaps(const std::vector<domain::report_trade_market_gap>& trade_gaps);

    /**
     * @brief Deletes a trade gap by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_gap(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade gaps by their primary keys.
     */
    void delete_trade_gaps(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade gap.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_trade_market_gap> get_trade_gap_history(const std::string& id);

private:
    context ctx_;
    repository::report_trade_market_gap_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::reporting::messaging::report_trade_market_gap_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::report_trade_market_gap>& current,
                   domain::report_trade_market_gap& out);
};

}

#endif
