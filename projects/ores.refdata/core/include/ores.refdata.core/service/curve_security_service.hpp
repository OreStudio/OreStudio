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
#ifndef ORES_REFDATA_CORE_SERVICE_CURVE_SECURITY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURVE_SECURITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/curve_security.hpp"
#include "ores.refdata.api/messaging/curve_security_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/curve_security_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing curve securities.
 *
 * Provides a higher-level interface for curve security operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT curve_security_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.curve_security_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a curve_security_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit curve_security_service(context ctx);

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
    messaging::list_curve_securities_response
    list_curve_securities(const messaging::list_curve_securities_request& request);
    messaging::get_curve_security_response
    get_curve_security(const messaging::get_curve_security_request& request);
    messaging::get_many_curve_securities_response
    get_many_curve_securities(const messaging::get_many_curve_securities_request& request);
    messaging::put_curve_security_response
    put_curve_security(const messaging::put_curve_security_request& request);
    messaging::put_many_curve_securities_response
    put_many_curve_securities(const messaging::put_many_curve_securities_request& request);
    messaging::delete_curve_security_response
    delete_curve_security(const messaging::delete_curve_security_request& request);
    messaging::delete_many_curve_securities_response
    delete_many_curve_securities(const messaging::delete_many_curve_securities_request& request);
    messaging::list_curve_security_versions_response
    list_curve_security_versions(const messaging::list_curve_security_versions_request& request);
    messaging::get_curve_security_version_response
    get_curve_security_version(const messaging::get_curve_security_version_request& request);
    /**@}*/

    /**
     * @brief Lists curve securities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of curve securities for the requested page.
     */
    std::vector<domain::curve_security> list_securities(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active curve securities.
     *
     * @return Total number of active curve securities.
     */
    std::uint32_t count_securities();


    /**
     * @brief Retrieves a single curve security as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The curve security at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_security> get_security_at_version(const boost::uuids::uuid& id,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single curve security by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The curve security if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_security> get_security(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of curve securities by primary key.
     */
    std::vector<domain::curve_security> get_securities(const std::vector<std::string>& ids);

    /**
     * @brief Saves a curve security (creates or updates).
     *
     * @param security The curve security to save.
     * @throws std::exception on failure.
     */
    void save_security(const domain::curve_security& security);

    /**
     * @brief Saves a batch of curve securities.
     *
     * @param securities The curve securities to save.
     * @throws std::exception on failure.
     */
    void save_securities(const std::vector<domain::curve_security>& securities);

    /**
     * @brief Deletes a curve security by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_security(const boost::uuids::uuid& id);

    /**
     * @brief Deletes curve securities by their primary keys.
     */
    void delete_securities(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a curve security.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::curve_security> get_security_history(const std::string& id);

private:
    context ctx_;
    repository::curve_security_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::curve_security_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::curve_security& out);
};

}

#endif
