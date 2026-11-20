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
#ifndef ORES_TRADING_CORE_SERVICE_CBO_BASKET_MEMBER_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_CBO_BASKET_MEMBER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/domain/cbo_basket_member.hpp"
#include "ores.trading.api/messaging/cbo_basket_member_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/cbo_basket_member_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing cbo basket members.
 *
 * Provides a higher-level interface for cbo basket member operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT cbo_basket_member_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.cbo_basket_member_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cbo_basket_member_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cbo_basket_member_service(const context& ctx);

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
    ::ores::trading::messaging::list_cbo_basket_members_response list_cbo_basket_members(
        const ::ores::trading::messaging::list_cbo_basket_members_request& request);
    ::ores::trading::messaging::get_cbo_basket_member_response
    get_cbo_basket_member(const ::ores::trading::messaging::get_cbo_basket_member_request& request);
    ::ores::trading::messaging::get_many_cbo_basket_members_response get_many_cbo_basket_members(
        const ::ores::trading::messaging::get_many_cbo_basket_members_request& request);
    ::ores::trading::messaging::put_cbo_basket_member_response
    put_cbo_basket_member(const ::ores::trading::messaging::put_cbo_basket_member_request& request);
    ::ores::trading::messaging::put_many_cbo_basket_members_response put_many_cbo_basket_members(
        const ::ores::trading::messaging::put_many_cbo_basket_members_request& request);
    ::ores::trading::messaging::delete_cbo_basket_member_response delete_cbo_basket_member(
        const ::ores::trading::messaging::delete_cbo_basket_member_request& request);
    ::ores::trading::messaging::delete_many_cbo_basket_members_response
    delete_many_cbo_basket_members(
        const ::ores::trading::messaging::delete_many_cbo_basket_members_request& request);
    ::ores::trading::messaging::list_by_trade_id_cbo_basket_members_response
    list_by_trade_id_cbo_basket_members(
        const ::ores::trading::messaging::list_by_trade_id_cbo_basket_members_request& request);
    ::ores::trading::messaging::list_cbo_basket_member_versions_response
    list_cbo_basket_member_versions(
        const ::ores::trading::messaging::list_cbo_basket_member_versions_request& request);
    ::ores::trading::messaging::get_cbo_basket_member_version_response
    get_cbo_basket_member_version(
        const ::ores::trading::messaging::get_cbo_basket_member_version_request& request);
    /**@}*/

    /**
     * @brief Lists cbo basket members with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cbo basket members for the requested page.
     */
    std::vector<domain::cbo_basket_member> list_cbo_basket_members(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active cbo basket members.
     *
     * @return Total number of active cbo basket members.
     */
    std::uint32_t count_cbo_basket_members();


    /**
     * @brief Lists cbo basket members filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cbo basket members for the requested page.
     */
    std::vector<domain::cbo_basket_member> list_cbo_basket_members_by_trade_id(
        const std::string& trade_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cbo basket members filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching cbo basket members.
     */
    std::uint32_t count_cbo_basket_members_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single cbo basket member as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cbo basket member at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cbo_basket_member> get_cbo_basket_member_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single cbo basket member by its primary key.
     *
     * @return The cbo basket member if found, std::nullopt otherwise.
     */
    std::optional<domain::cbo_basket_member>
    get_cbo_basket_member(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of cbo basket members by primary key.
     */
    std::vector<domain::cbo_basket_member>
    get_cbo_basket_members(const std::vector<std::string>& trade_ids,
                           const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a cbo basket member (creates or updates).
     *
     * @param cbo_basket_member The cbo basket member to save.
     * @throws std::exception on failure.
     */
    void save_cbo_basket_member(const domain::cbo_basket_member& cbo_basket_member);

    /**
     * @brief Saves a batch of cbo basket members.
     *
     * @param cbo_basket_members The cbo basket members to save.
     * @throws std::exception on failure.
     */
    void save_cbo_basket_members(const std::vector<domain::cbo_basket_member>& cbo_basket_members);

    /**
     * @brief Deletes a cbo basket member by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cbo_basket_member(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Deletes cbo basket members by their primary keys.
     */
    void delete_cbo_basket_members(const std::vector<std::string>& trade_ids,
                                   const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a cbo basket member.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cbo_basket_member>
    get_cbo_basket_member_history(const std::string& trade_id, const std::string& sequence_number);

private:
    context ctx_;
    repository::cbo_basket_member_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::cbo_basket_member_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::cbo_basket_member>& current,
                   domain::cbo_basket_member& out);
};

}

#endif
