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
#ifndef ORES_ORGMODE_CORE_SERVICE_KNOWLEDGE_PAGE_SERVICE_HPP
#define ORES_ORGMODE_CORE_SERVICE_KNOWLEDGE_PAGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.orgmode.api/domain/knowledge_page.hpp"
#include "ores.orgmode.api/messaging/knowledge_page_protocol.hpp"
#include "ores.orgmode.core/export.hpp"
#include "ores.orgmode.core/repository/knowledge_page_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::orgmode::service {

/**
 * @brief Service for managing knowledge pages.
 *
 * Provides a higher-level interface for knowledge page operations,
 * wrapping the underlying repository.
 */
class ORES_ORGMODE_CORE_EXPORT knowledge_page_service {
private:
    inline static std::string_view logger_name = "ores.orgmode.service.knowledge_page_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a knowledge_page_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit knowledge_page_service(const context& ctx);

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
    ::ores::orgmode::messaging::list_knowledge_pages_response
    list_knowledge_pages(const ::ores::orgmode::messaging::list_knowledge_pages_request& request);
    ::ores::orgmode::messaging::get_knowledge_page_response
    get_knowledge_page(const ::ores::orgmode::messaging::get_knowledge_page_request& request);
    ::ores::orgmode::messaging::get_many_knowledge_pages_response get_many_knowledge_pages(
        const ::ores::orgmode::messaging::get_many_knowledge_pages_request& request);
    ::ores::orgmode::messaging::list_knowledge_page_versions_response list_knowledge_page_versions(
        const ::ores::orgmode::messaging::list_knowledge_page_versions_request& request);
    ::ores::orgmode::messaging::get_knowledge_page_version_response get_knowledge_page_version(
        const ::ores::orgmode::messaging::get_knowledge_page_version_request& request);
    /**@}*/

    /**
     * @brief Lists knowledge pages with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of knowledge pages for the requested page.
     */
    std::vector<domain::knowledge_page> list_pages(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active knowledge pages.
     *
     * @return Total number of active knowledge pages.
     */
    std::uint32_t count_pages();


    /**
     * @brief Retrieves a single knowledge page as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The knowledge page at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::knowledge_page> get_page_at_version(const std::string& id,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single knowledge page by its primary key.
     *
     * @return The knowledge page if found, std::nullopt otherwise.
     */
    std::optional<domain::knowledge_page> get_page(const std::string& id);

    /**
     * @brief Retrieves a batch of knowledge pages by primary key.
     */
    std::vector<domain::knowledge_page> get_pages(const std::vector<std::string>& ids);

    /**
     * @brief Saves a knowledge page (creates or updates).
     *
     * @param page The knowledge page to save.
     * @throws std::exception on failure.
     */
    void save_page(const domain::knowledge_page& page);

    /**
     * @brief Saves a batch of knowledge pages.
     *
     * @param pages The knowledge pages to save.
     * @throws std::exception on failure.
     */
    void save_pages(const std::vector<domain::knowledge_page>& pages);

    /**
     * @brief Deletes a knowledge page by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_page(const std::string& id);

    /**
     * @brief Deletes knowledge pages by their primary keys.
     */
    void delete_pages(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a knowledge page.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::knowledge_page> get_page_history(const std::string& id);

private:
    context ctx_;
    repository::knowledge_page_repository repo_;
};

}

#endif
