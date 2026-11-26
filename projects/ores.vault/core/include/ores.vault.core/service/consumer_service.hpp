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
#ifndef ORES_VAULT_CORE_SERVICE_CONSUMER_SERVICE_HPP
#define ORES_VAULT_CORE_SERVICE_CONSUMER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.vault.api/domain/consumer.hpp"
#include "ores.vault.api/messaging/consumer_protocol.hpp"
#include "ores.vault.core/export.hpp"
#include "ores.vault.core/repository/consumer_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::vault::service {

/**
 * @brief Service for managing consumers.
 *
 * Provides a higher-level interface for consumer operations,
 * wrapping the underlying repository.
 */
class ORES_VAULT_CORE_EXPORT consumer_service {
private:
    inline static std::string_view logger_name = "ores.vault.service.consumer_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a consumer_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit consumer_service(const context& ctx);

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
    ::ores::vault::messaging::list_consumers_response
    list_consumers(const ::ores::vault::messaging::list_consumers_request& request);
    ::ores::vault::messaging::get_consumer_response
    get_consumer(const ::ores::vault::messaging::get_consumer_request& request);
    ::ores::vault::messaging::get_many_consumers_response
    get_many_consumers(const ::ores::vault::messaging::get_many_consumers_request& request);
    ::ores::vault::messaging::put_consumer_response
    put_consumer(const ::ores::vault::messaging::put_consumer_request& request);
    ::ores::vault::messaging::put_many_consumers_response
    put_many_consumers(const ::ores::vault::messaging::put_many_consumers_request& request);
    ::ores::vault::messaging::delete_consumer_response
    delete_consumer(const ::ores::vault::messaging::delete_consumer_request& request);
    ::ores::vault::messaging::delete_many_consumers_response
    delete_many_consumers(const ::ores::vault::messaging::delete_many_consumers_request& request);
    ::ores::vault::messaging::list_consumer_versions_response
    list_consumer_versions(const ::ores::vault::messaging::list_consumer_versions_request& request);
    ::ores::vault::messaging::get_consumer_version_response
    get_consumer_version(const ::ores::vault::messaging::get_consumer_version_request& request);
    /**@}*/

    /**
     * @brief Lists consumers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of consumers for the requested page.
     */
    std::vector<domain::consumer> list_consumers(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active consumers.
     *
     * @return Total number of active consumers.
     */
    std::uint32_t count_consumers();


    /**
     * @brief Retrieves a single consumer as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The consumer at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::consumer> get_consumer_at_version(const boost::uuids::uuid& id,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single consumer by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The consumer if found, std::nullopt otherwise.
     */
    std::optional<domain::consumer> get_consumer(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single consumer by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The consumer if found, std::nullopt otherwise.
     */
    std::optional<domain::consumer> get_consumer_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of consumers by primary key.
     */
    std::vector<domain::consumer> get_consumers(const std::vector<std::string>& ids);

    /**
     * @brief Saves a consumer (creates or updates).
     *
     * @param consumer The consumer to save.
     * @throws std::exception on failure.
     */
    void save_consumer(const domain::consumer& consumer);

    /**
     * @brief Saves a batch of consumers.
     *
     * @param consumers The consumers to save.
     * @throws std::exception on failure.
     */
    void save_consumers(const std::vector<domain::consumer>& consumers);

    /**
     * @brief Deletes a consumer by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_consumer(const boost::uuids::uuid& id);

    /**
     * @brief Deletes consumers by their primary keys.
     */
    void delete_consumers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a consumer.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::consumer> get_consumer_history(const std::string& key);

private:
    context ctx_;
    repository::consumer_repository repo_;

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
    prepare_change(const ::ores::vault::messaging::consumer_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::consumer>& current,
                   domain::consumer& out);
};

}

#endif
