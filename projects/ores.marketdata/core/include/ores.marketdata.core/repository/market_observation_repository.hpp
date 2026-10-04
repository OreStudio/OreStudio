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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_OBSERVATION_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_OBSERVATION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes market observations to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT market_observation_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.market_observation_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes market observations to database.
     *
     * The plain form replaces the row the caller last read: it states the
     * version the row carries now, so the store can tell a replace from a
     * create. A row that moved on since that read is a conflict, never a silent
     * overwrite.
     */
    /**@{*/
    void write(context ctx, const domain::market_observation& v);
    void write(context ctx, const std::vector<domain::market_observation>& v);
    /**@}*/

    /**
     * @brief Writes a market observation, honouring the claim it states.
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
               const domain::market_observation& v,
               const ores::utility::domain::precondition& claim);

    /**
     * @brief Writes a set of market observations, each honouring its own
     * claim, as one statement.
     */
    void write(context ctx,
               const std::vector<domain::market_observation>& v,
               const std::vector<ores::utility::domain::precondition>& claims);

    /**
     * @brief Reads latest market observations, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::market_observation> read_latest(context ctx);
    std::vector<domain::market_observation> read_latest(context ctx, const std::string& id);
    std::vector<domain::market_observation> read_latest(context ctx,
                                                        const std::vector<std::string>& ids);
    /**@}*/


    /**
     * @brief Reads all market observations, possibly filtered by primary key.
     */
    std::vector<domain::market_observation> read_all(context ctx, const std::string& id);


    /**
     * @brief Reads latest market observations filtered by series_id, with pagination.
     * @param ctx Repository context with database connection
     * @param series_id The series_id to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     */
    std::vector<domain::market_observation> read_latest_by_series_id(
        context ctx,
        const std::string& series_id,
        std::uint32_t offset,
        std::uint32_t limit,
        const ores::utility::domain::order& order = {},
        const std::optional<messaging::market_observations_filter>& filter = std::nullopt);

    /**
     * @brief Gets the total count of active market observations filtered by series_id.
     */
    std::uint32_t get_total_market_observation_count_by_series_id(
        context ctx,
        const std::string& series_id,
        const std::optional<messaging::market_observations_filter>& filter = std::nullopt);


    /**
     * @brief Whether a list of market observations can be ordered by a field.
     *
     * The model's :sortable: columns, and nothing else.
     */
    static bool is_sortable(std::string_view field);

    /**
     * @brief Reads latest market observations with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @param order The stated order; an empty field is the default order
     * @param filter The filter record; the members it sets must all hold
     * @throws std::invalid_argument if the field is not sortable
     */
    std::vector<domain::market_observation>
    read_latest(context ctx,
                std::uint32_t offset,
                std::uint32_t limit,
                const ores::utility::domain::order& order = {},
                const std::optional<messaging::market_observations_filter>& filter = std::nullopt);

    /**
     * @brief Gets the total count of active market observations.
     * @param ctx Repository context with database connection
     * @return Total number of active market observations
     */
    std::uint32_t get_total_market_observation_count(
        context ctx,
        const std::optional<messaging::market_observations_filter>& filter = std::nullopt);

    /**
     * @brief Deletes a market observation by closing its temporal validity.
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
     * @brief Removes a market observation, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(context ctx, const std::string& id, std::optional<std::uint32_t> version);

    /**
     * @brief Deletes market observations by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);

    void insert(context ctx, const domain::market_observation& v);
    void insert(context ctx, const std::vector<domain::market_observation>& v);

    std::vector<domain::market_observation>
    read_latest_for_series(context ctx, const boost::uuids::uuid& series_id);

    std::vector<domain::market_observation>
    read_as_of(context ctx,
               const boost::uuids::uuid& series_id,
               const std::chrono::system_clock::time_point& as_of_datetime);

    std::vector<std::vector<domain::market_observation>>
    read_as_of_buckets(context ctx,
                       const boost::uuids::uuid& series_id,
                       const std::chrono::system_clock::time_point& latest_boundary,
                       const std::chrono::seconds& bucket_size,
                       unsigned int bucket_count);

private:
    /**
     * @brief The claim a replace makes: the version the row carries now, or
     * that no row exists yet.
     */
    ores::utility::domain::precondition replace_claim(context ctx,
                                                      const domain::market_observation& v);

    /**
     * @brief The object with the claim's version stamped onto it.
     *
     * A claim the store cannot check is refused here rather than ignored.
     */
    domain::market_observation apply_claim(context ctx,
                                           const domain::market_observation& v,
                                           const ores::utility::domain::precondition& claim);
};

}

#endif
