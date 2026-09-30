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
#ifndef ORES_REFDATA_CORE_SERVICE_CURVE_QUOTE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURVE_QUOTE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/curve_quote.hpp"
#include "ores.refdata.api/messaging/curve_quote_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/curve_quote_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing curve quotes.
 *
 * Provides a higher-level interface for curve quote operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT curve_quote_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.curve_quote_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a curve_quote_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit curve_quote_service(context ctx);

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
    messaging::list_curve_quotes_response
    list_curve_quotes(const messaging::list_curve_quotes_request& request);
    messaging::get_curve_quote_response
    get_curve_quote(const messaging::get_curve_quote_request& request);
    messaging::get_many_curve_quotes_response
    get_many_curve_quotes(const messaging::get_many_curve_quotes_request& request);
    messaging::put_curve_quote_response
    put_curve_quote(const messaging::put_curve_quote_request& request);
    messaging::put_many_curve_quotes_response
    put_many_curve_quotes(const messaging::put_many_curve_quotes_request& request);
    messaging::delete_curve_quote_response
    delete_curve_quote(const messaging::delete_curve_quote_request& request);
    messaging::delete_many_curve_quotes_response
    delete_many_curve_quotes(const messaging::delete_many_curve_quotes_request& request);
    messaging::list_curve_quote_versions_response
    list_curve_quote_versions(const messaging::list_curve_quote_versions_request& request);
    messaging::get_curve_quote_version_response
    get_curve_quote_version(const messaging::get_curve_quote_version_request& request);
    /**@}*/

    /**
     * @brief Lists curve quotes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of curve quotes for the requested page.
     */
    std::vector<domain::curve_quote> list_quotes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active curve quotes.
     *
     * @return Total number of active curve quotes.
     */
    std::uint32_t count_quotes();


    /**
     * @brief Retrieves a single curve quote as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The curve quote at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_quote> get_quote_at_version(const boost::uuids::uuid& id,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single curve quote by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The curve quote if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_quote> get_quote(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of curve quotes by primary key.
     */
    std::vector<domain::curve_quote> get_quotes(const std::vector<std::string>& ids);

    /**
     * @brief Saves a curve quote (creates or updates).
     *
     * @param quote The curve quote to save.
     * @throws std::exception on failure.
     */
    void save_quote(const domain::curve_quote& quote);

    /**
     * @brief Saves a batch of curve quotes.
     *
     * @param quotes The curve quotes to save.
     * @throws std::exception on failure.
     */
    void save_quotes(const std::vector<domain::curve_quote>& quotes);

    /**
     * @brief Deletes a curve quote by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_quote(const boost::uuids::uuid& id);

    /**
     * @brief Deletes curve quotes by their primary keys.
     */
    void delete_quotes(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a curve quote.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::curve_quote> get_quote_history(const std::string& id);

private:
    context ctx_;
    repository::curve_quote_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::curve_quote_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::curve_quote& out);
};

}

#endif
