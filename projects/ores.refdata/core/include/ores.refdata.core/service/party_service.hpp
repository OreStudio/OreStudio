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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing parties.
 *
 * Provides a higher-level interface for party operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_service(context ctx);

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
    messaging::list_parties_response list_parties(const messaging::list_parties_request& request);
    messaging::get_party_response get_party(const messaging::get_party_request& request);
    messaging::get_many_parties_response
    get_many_parties(const messaging::get_many_parties_request& request);
    messaging::put_party_response put_party(const messaging::put_party_request& request);
    messaging::put_many_parties_response
    put_many_parties(const messaging::put_many_parties_request& request);
    messaging::delete_party_response delete_party(const messaging::delete_party_request& request);
    messaging::delete_many_parties_response
    delete_many_parties(const messaging::delete_many_parties_request& request);
    messaging::list_party_versions_response
    list_party_versions(const messaging::list_party_versions_request& request);
    messaging::get_party_version_response
    get_party_version(const messaging::get_party_version_request& request);
    /**@}*/

    /**
     * @brief Lists parties with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of parties for the requested page.
     */
    std::vector<domain::party> list_parties(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active parties.
     *
     * @return Total number of active parties.
     */
    std::uint32_t count_parties();


    /**
     * @brief Retrieves a single party as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The party at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party> get_party_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single party by its primary key.
     *
     * @return The party if found, std::nullopt otherwise.
     */
    std::optional<domain::party> get_party(const std::string& id);

    /**
     * @brief Retrieves a single party by its uuid primary key.
     *
     * @return The party if found, std::nullopt otherwise.
     */
    std::optional<domain::party> find_party(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single party by its short_code.
     *
     * @return The party if found, std::nullopt otherwise.
     */
    std::optional<domain::party> find_party_by_code(const std::string& short_code);

    /**
     * @brief Retrieves a batch of parties by primary key.
     */
    std::vector<domain::party> get_parties(const std::vector<std::string>& ids);

    /**
     * @brief Saves a party (creates or updates).
     *
     * @param party The party to save.
     * @throws std::exception on failure.
     */
    void save_party(const domain::party& party);

    /**
     * @brief Saves a batch of parties.
     *
     * @param parties The parties to save.
     * @throws std::exception on failure.
     */
    void save_parties(const std::vector<domain::party>& parties);

    /**
     * @brief Deletes a party by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_party(const std::string& id);

    /**
     * @brief Removes a party by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_party(const boost::uuids::uuid& id);

    /**
     * @brief Deletes parties by their primary keys.
     */
    void delete_parties(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a party.
     */
    std::vector<domain::party> get_party_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a party
     * by its uuid primary key.
     */
    std::vector<domain::party> get_party_history(const boost::uuids::uuid& id);

    /**
     * @brief Gets the party hierarchy (as a forest of trees) rooted
     * at, or containing, the given party.
     *
     * @param root_id The party to start from.
     * @param from_root If true, returns the whole tree the given node
     * belongs to instead of just its subtree.
     * @return A forest of hierarchy_node trees (normally a single root).
     */
    std::vector<ores::utility::domain::hierarchy_node>
    get_hierarchy(const boost::uuids::uuid& root_id, bool from_root);

private:
    context ctx_;
    repository::party_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::party_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::party& out);
};

}

#endif
