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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_XML_DOCUMENT_KIND_HPP
#define ORES_ORE_CORE_XML_DOCUMENT_KIND_HPP

#include "ores.ore.core/export.hpp"
#include <filesystem>
#include <optional>

namespace ores::ore::xml {

/**
 * @brief The ORE document kinds the XML facet reads and writes.
 */
enum class document_kind {
    portfolio,
    currency_config,
    calendar_adjustments,
    conventions,
};

/**
 * @brief Names the document kind a file holds, from its root element.
 *
 * The root element alone decides. The rule is shared: the round trip and
 * the import directory scanner both ask this function, so a file cannot be
 * one kind to one caller and another kind to the other. Matching a keyword
 * in the opening bytes instead would both miss a root element spelled with
 * attributes and match a keyword inside a comment or a later element.
 *
 * @param path Path to the file
 * @return The kind, or @c std::nullopt when the root element is not one of
 * them, or the file cannot be read
 */
ORES_ORE_CORE_EXPORT std::optional<document_kind>
detect_document_kind(const std::filesystem::path& path);

}

#endif
