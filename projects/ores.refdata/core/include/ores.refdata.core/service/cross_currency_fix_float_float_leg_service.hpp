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
#ifndef ORES_REFDATA_CORE_SERVICE_CROSS_CURRENCY_FIX_FLOAT_FLOAT_LEG_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CROSS_CURRENCY_FIX_FLOAT_FLOAT_LEG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_float_leg.hpp"
#include "ores.refdata.api/messaging/cross_currency_fix_float_float_leg_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_float_leg_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing cross currency fix float float legs.
 *
 * Provides a higher-level interface for cross currency fix float float leg operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT cross_currency_fix_float_float_leg_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.cross_currency_fix_float_float_leg_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cross_currency_fix_float_float_leg_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cross_currency_fix_float_float_leg_service(context ctx);

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
    ::ores::refdata::messaging::list_cross_currency_fix_float_float_legs_response
    list_cross_currency_fix_float_float_legs(
        const ::ores::refdata::messaging::list_cross_currency_fix_float_float_legs_request&
            request);
    ::ores::refdata::messaging::get_cross_currency_fix_float_float_leg_response
    get_cross_currency_fix_float_float_leg(
        const ::ores::refdata::messaging::get_cross_currency_fix_float_float_leg_request& request);
    ::ores::refdata::messaging::get_many_cross_currency_fix_float_float_legs_response
    get_many_cross_currency_fix_float_float_legs(
        const ::ores::refdata::messaging::get_many_cross_currency_fix_float_float_legs_request&
            request);
    ::ores::refdata::messaging::put_cross_currency_fix_float_float_leg_response
    put_cross_currency_fix_float_float_leg(
        const ::ores::refdata::messaging::put_cross_currency_fix_float_float_leg_request& request);
    ::ores::refdata::messaging::put_many_cross_currency_fix_float_float_legs_response
    put_many_cross_currency_fix_float_float_legs(
        const ::ores::refdata::messaging::put_many_cross_currency_fix_float_float_legs_request&
            request);
    ::ores::refdata::messaging::delete_cross_currency_fix_float_float_leg_response
    delete_cross_currency_fix_float_float_leg(
        const ::ores::refdata::messaging::delete_cross_currency_fix_float_float_leg_request&
            request);
    ::ores::refdata::messaging::delete_many_cross_currency_fix_float_float_legs_response
    delete_many_cross_currency_fix_float_float_legs(
        const ::ores::refdata::messaging::delete_many_cross_currency_fix_float_float_legs_request&
            request);
    ::ores::refdata::messaging::list_by_convention_id_cross_currency_fix_float_float_legs_response
    list_by_convention_id_cross_currency_fix_float_float_legs(
        const ::ores::refdata::messaging::
            list_by_convention_id_cross_currency_fix_float_float_legs_request& request);
    ::ores::refdata::messaging::list_cross_currency_fix_float_float_leg_versions_response
    list_cross_currency_fix_float_float_leg_versions(
        const ::ores::refdata::messaging::list_cross_currency_fix_float_float_leg_versions_request&
            request);
    ::ores::refdata::messaging::get_cross_currency_fix_float_float_leg_version_response
    get_cross_currency_fix_float_float_leg_version(
        const ::ores::refdata::messaging::get_cross_currency_fix_float_float_leg_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists cross currency fix float float legs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cross currency fix float float legs for the requested page.
     */
    std::vector<domain::cross_currency_fix_float_float_leg>
    list_cross_currency_fix_float_float_legs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cross currency fix float float legs.
     *
     * @return Total number of active cross currency fix float float legs.
     */
    std::uint32_t count_cross_currency_fix_float_float_legs();


    /**
     * @brief Lists cross currency fix float float legs filtered by convention_id, with pagination.
     *
     * @param convention_id The convention_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cross currency fix float float legs for the requested page.
     */
    std::vector<domain::cross_currency_fix_float_float_leg>
    list_cross_currency_fix_float_float_legs_by_convention_id(const std::string& convention_id,
                                                              std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active cross currency fix float float legs filtered by
     * convention_id.
     *
     * @param convention_id The convention_id to filter by.
     * @return Total number of matching cross currency fix float float legs.
     */
    std::uint32_t
    count_cross_currency_fix_float_float_legs_by_convention_id(const std::string& convention_id);

    /**
     * @brief Lists cross currency fix float float legs filtered by convention_id, with pagination.
     *
     * @param convention_id The convention_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cross currency fix float float legs for the requested page.
     */
    std::vector<domain::cross_currency_fix_float_float_leg>
    list_cross_currency_fix_float_float_legs_by_convention_id(
        const boost::uuids::uuid& convention_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cross currency fix float float legs filtered by
     * convention_id.
     *
     * @param convention_id The convention_id to filter by.
     * @return Total number of matching cross currency fix float float legs.
     */
    std::uint32_t count_cross_currency_fix_float_float_legs_by_convention_id(
        const boost::uuids::uuid& convention_id);


    /**
     * @brief Lists cross currency fix float float legs filtered by convention_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param convention_id The convention_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching cross currency fix float float legs.
     */
    std::vector<domain::cross_currency_fix_float_float_leg>
    list_cross_currency_fix_float_float_legs_by_convention_id_as_of(
        const std::string& convention_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single cross currency fix float float leg as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cross currency fix float float leg at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::cross_currency_fix_float_float_leg>
    get_cross_currency_fix_float_float_leg_at_version(const boost::uuids::uuid& id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single cross currency fix float float leg by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The cross currency fix float float leg if found, std::nullopt otherwise.
     */
    std::optional<domain::cross_currency_fix_float_float_leg>
    get_cross_currency_fix_float_float_leg(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of cross currency fix float float legs by primary key.
     */
    std::vector<domain::cross_currency_fix_float_float_leg>
    get_cross_currency_fix_float_float_legs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a cross currency fix float float leg (creates or updates).
     *
     * @param cross_currency_fix_float_float_leg The cross currency fix float float leg to save.
     * @throws std::exception on failure.
     */
    void save_cross_currency_fix_float_float_leg(
        const domain::cross_currency_fix_float_float_leg& cross_currency_fix_float_float_leg);

    /**
     * @brief Saves a batch of cross currency fix float float legs.
     *
     * @param cross_currency_fix_float_float_legs The cross currency fix float float legs to save.
     * @throws std::exception on failure.
     */
    void save_cross_currency_fix_float_float_legs(
        const std::vector<domain::cross_currency_fix_float_float_leg>&
            cross_currency_fix_float_float_legs);

    /**
     * @brief Deletes a cross currency fix float float leg by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cross_currency_fix_float_float_leg(const boost::uuids::uuid& id);

    /**
     * @brief Deletes cross currency fix float float legs by their primary keys.
     */
    void delete_cross_currency_fix_float_float_legs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a cross currency fix float float leg.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cross_currency_fix_float_float_leg>
    get_cross_currency_fix_float_float_leg_history(const std::string& id);

private:
    context ctx_;
    repository::cross_currency_fix_float_float_leg_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(
        const ::ores::refdata::messaging::cross_currency_fix_float_float_leg_change& change,
        const ores::utility::domain::change_intent& intent,
        domain::cross_currency_fix_float_float_leg& out);
};

}

#endif
