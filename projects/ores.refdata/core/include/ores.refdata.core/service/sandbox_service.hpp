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
#ifndef ORES_REFDATA_CORE_SERVICE_SANDBOX_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_SANDBOX_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/sandbox.hpp"
#include "ores.refdata.api/messaging/sandbox_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/sandbox_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing sandboxes.
 *
 * Provides a higher-level interface for sandbox operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT sandbox_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.sandbox_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a sandbox_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit sandbox_service(context ctx);

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
    messaging::list_sandboxes_response
    list_sandboxes(const messaging::list_sandboxes_request& request);
    messaging::get_sandbox_response get_sandbox(const messaging::get_sandbox_request& request);
    messaging::get_many_sandboxes_response
    get_many_sandboxes(const messaging::get_many_sandboxes_request& request);
    messaging::put_sandbox_response put_sandbox(const messaging::put_sandbox_request& request);
    messaging::put_many_sandboxes_response
    put_many_sandboxes(const messaging::put_many_sandboxes_request& request);
    messaging::delete_sandbox_response
    delete_sandbox(const messaging::delete_sandbox_request& request);
    messaging::delete_many_sandboxes_response
    delete_many_sandboxes(const messaging::delete_many_sandboxes_request& request);
    messaging::list_by_anchor_portfolio_id_sandboxes_response list_by_anchor_portfolio_id_sandboxes(
        const messaging::list_by_anchor_portfolio_id_sandboxes_request& request);
    messaging::list_sandbox_versions_response
    list_sandbox_versions(const messaging::list_sandbox_versions_request& request);
    messaging::get_sandbox_version_response
    get_sandbox_version(const messaging::get_sandbox_version_request& request);
    /**@}*/

    /**
     * @brief Lists sandboxes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of sandboxes for the requested page.
     */
    std::vector<domain::sandbox> list_sandboxes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandboxes.
     *
     * @return Total number of active sandboxes.
     */
    std::uint32_t count_sandboxes();


    /**
     * @brief Lists sandboxes filtered by anchor_portfolio_id, with pagination.
     *
     * @param anchor_portfolio_id The anchor_portfolio_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching sandboxes for the requested page.
     */
    std::vector<domain::sandbox> list_sandboxes_by_anchor_portfolio_id(
        const std::string& anchor_portfolio_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandboxes filtered by anchor_portfolio_id.
     *
     * @param anchor_portfolio_id The anchor_portfolio_id to filter by.
     * @return Total number of matching sandboxes.
     */
    std::uint32_t count_sandboxes_by_anchor_portfolio_id(const std::string& anchor_portfolio_id);

    /**
     * @brief Lists sandboxes filtered by anchor_portfolio_id, with pagination.
     *
     * @param anchor_portfolio_id The anchor_portfolio_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching sandboxes for the requested page.
     */
    std::vector<domain::sandbox> list_sandboxes_by_anchor_portfolio_id(
        const boost::uuids::uuid& anchor_portfolio_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sandboxes filtered by anchor_portfolio_id.
     *
     * @param anchor_portfolio_id The anchor_portfolio_id to filter by.
     * @return Total number of matching sandboxes.
     */
    std::uint32_t
    count_sandboxes_by_anchor_portfolio_id(const boost::uuids::uuid& anchor_portfolio_id);


    /**
     * @brief Retrieves a single sandbox as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The sandbox at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox> get_sandbox_at_version(const boost::uuids::uuid& id,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single sandbox by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The sandbox if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox> get_sandbox(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single sandbox by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The sandbox if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox> get_sandbox_by_name(const std::string& name);

    /**
     * @brief Retrieves a single sandbox by its uuid primary key.
     *
     * @return The sandbox if found, std::nullopt otherwise.
     */
    std::optional<domain::sandbox> find_sandbox(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of sandboxes by primary key.
     */
    std::vector<domain::sandbox> get_sandboxes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a sandbox (creates or updates).
     *
     * @param sandbox The sandbox to save.
     * @throws std::exception on failure.
     */
    void save_sandbox(const domain::sandbox& sandbox);

    /**
     * @brief Saves a batch of sandboxes.
     *
     * @param sandboxes The sandboxes to save.
     * @throws std::exception on failure.
     */
    void save_sandboxes(const std::vector<domain::sandbox>& sandboxes);

    /**
     * @brief Deletes a sandbox by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_sandbox(const boost::uuids::uuid& id);

    /**
     * @brief Removes a sandbox by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_sandbox(const boost::uuids::uuid& id);

    /**
     * @brief Deletes sandboxes by their primary keys.
     */
    void delete_sandboxes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a sandbox.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::sandbox> get_sandbox_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a sandbox
     * by its uuid primary key.
     */
    std::vector<domain::sandbox> get_sandbox_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::sandbox_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::sandbox_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::sandbox& out);
};

}

#endif
