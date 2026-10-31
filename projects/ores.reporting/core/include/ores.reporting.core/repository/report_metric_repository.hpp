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
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_CORE_REPOSITORY_REPORT_METRIC_REPOSITORY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_REPORT_METRIC_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_metric.hpp"
#include "ores.reporting.api/messaging/report_metric_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::repository {

/**
 * @brief Reads and writes report metrics to data storage.
 */
class ORES_REPORTING_CORE_EXPORT report_metric_repository {
private:
    inline static std::string_view logger_name =
        "ores.reporting.repository.report_metric_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the insert statement the repository writes.
     */
    std::string sql();

    /**
     * @brief Writes report metrics to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(context ctx, const domain::report_metric& v);
    void write(context ctx, const std::vector<domain::report_metric>& v);
    /**@}*/

    /**
     * @brief Writes a report metric, honouring the claim it states.
     *
     * The claim is the version the caller read (@c must_match_version), that no
     * current row exists (@c must_not_exist), or neither (@c any, which
     * replaces the row as it stands). The store decides in the write's own
     * transaction, so a create that collides with a live row and a write over a
     * row that moved on are refused by the store rather than by a check a
     * caller might have forgotten.
     *
     * This table carries no version column, so the store cannot check a
     * version. A @c must_match_version claim is refused here, and a
     * @c must_not_exist claim over a live row is refused by the read below
     * rather than by the trigger.
     */
    void write(context ctx,
               const domain::report_metric& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of report metrics, each honouring its own
     * claim, as one statement.
     */
    void write(context ctx,
               const std::vector<domain::report_metric>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Appends report metrics without the claim a write makes.
     *
     * The table is an append-only series: a row is never edited in place, so
     * an insert reads no current row first. A bulk load is one statement, not
     * one claim query per row.
     */
    /**@{*/
    void insert(context ctx, const domain::report_metric& v);
    void insert(context ctx, const std::vector<domain::report_metric>& v);
    /**@}*/

    /**
     * @brief Reads latest report metrics, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::report_metric> read_latest(context ctx);
    std::vector<domain::report_metric> read_latest(context ctx, const std::string& id);
    std::vector<domain::report_metric> read_latest(context ctx,
                                                   const std::vector<std::string>& ids);
    /**@}*/


    /**
     * @brief Reads all report metrics, possibly filtered by primary key.
     */
    std::vector<domain::report_metric> read_all(context ctx, const std::string& id);


    /**
     * @brief Whether a list of report metrics can be ordered by a field.
     *
     * The model's :sortable: columns, and nothing else.
     */
    static bool is_sortable(std::string_view field);

    /**
     * @brief Reads latest report metrics with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     * @param filter The filter record; the members it sets must all hold
     * @throws std::invalid_argument if the field is not sortable
     */
    std::vector<domain::report_metric>
    read_latest(context ctx,
                std::uint32_t offset,
                std::uint32_t limit,
                const ores::utility::domain::order& order = {},
                const std::optional<messaging::report_metrics_filter>& filter = std::nullopt,
                const std::optional<std::string>& as_of = std::nullopt);

    /**
     * @brief Gets the total count of active report metrics.
     * @param ctx Repository context with database connection
     * @return Total number of active report metrics
     */
    std::uint32_t get_total_metric_count(
        context ctx,
        const std::optional<messaging::report_metrics_filter>& filter = std::nullopt,
        const std::optional<std::string>& as_of = std::nullopt);

    /**
     * @brief Deletes a report metric by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all -- a current-state
     * table has no version column, so a versioned removal has no meaning
     * there.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a report metric, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(context ctx, const std::string& id, std::optional<std::uint32_t> version);

    /**
     * @brief Deletes report metrics by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);


private:
    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(context ctx, const domain::report_metric& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::report_metric apply_claim(context ctx,
                                      const domain::report_metric& v,
                                      const ores::utility::domain::precondition& claim);
};

}

#endif
