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
#ifndef ORES_ORE_CORE_DOMAIN_PRICING_ENGINES_IMPORT_HPP
#define ORES_ORE_CORE_DOMAIN_PRICING_ENGINES_IMPORT_HPP

#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.ore.core/export.hpp"
#include <cstdint>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One product the file holds, as the review states it.
 *
 * A person compares the file against the configurations they hold by the type,
 * the model and the engine, so these are named rather than counted alone.
 */
struct pricing_engines_import_product {
    std::string type_code;
    std::string model;
    std::string engine;
    std::uint32_t parameters = 0;
};

/**
 * @brief Something in the file the system does not know, named so a person can
 * act on it.
 */
struct pricing_engines_import_issue {
    /** One of unknown_type, missing, dropped, duplicate. */
    std::string kind;
    std::string subject;
    std::string detail;
};

/**
 * @brief One row of the difference: added, changed or removed, and in words
 * what differs.
 *
 * =key= is the row's identity within its configuration — the type of its
 * product and which product of that type it is, then the scope and the name of
 * a setting — so a choice names a row and not a position in a list.
 */
struct pricing_engines_import_row {
    std::string key;
    /** One of product, parameter. */
    std::string entity;
    /** One of added, changed, removed. */
    std::string change;
    /** The product the row belongs to, or the configuration for a global setting. */
    std::string owner;
    /** The setting's name, empty for a product. */
    std::string name;
    std::string what;
};

/**
 * @brief A pricing engines document read from a file: the rows the tables can
 * hold, and everything the file stated that they cannot.
 */
struct pricing_engines_import_file {
    /** The root element the text states, or empty when it is not XML at all. */
    std::string root_element;
    /** Why the text is not this document, empty when it is. */
    std::string refusal;
    /** The file's own statement of its products, in the order it writes them. */
    std::vector<pricing_engines_import_product> products;
    /** The names of the root-level settings the file states. */
    std::vector<std::string> global_parameters;
    /** Everything the file states and the tables cannot hold. */
    std::vector<pricing_engines_import_issue> issues;
    /** The rows, as the analytics pricing engines document holds them. */
    ores::analytics::messaging::pricing_engines_document document;

    [[nodiscard]] bool ok() const noexcept {
        return refusal.empty();
    }
};

/**
 * @brief The difference between a file and the configuration it would be
 * written into.
 */
struct pricing_engines_import_difference {
    /** Rows the target holds with the file's values. */
    std::uint32_t held = 0;
    std::uint32_t added = 0;
    std::uint32_t changed = 0;
    std::uint32_t removed = 0;
    std::vector<pricing_engines_import_row> rows;
    /** What the vocabulary does not know about the file, such as a product type. */
    std::vector<pricing_engines_import_issue> issues;
};

/**
 * @brief The importer of the pricing engines document: reads a file into the
 * analytics pricing engines document, states what the tables cannot hold, and
 * says how the file differs from a configuration.
 *
 * The document is read with pugixml rather than by matching text, and it is
 * read in the order the exporter writes it — the products in their order, each
 * carrying Model, ModelParameters, Engine and EngineParameters, then one
 * GlobalParameters block — so a document exported and imported again comes back
 * product for product and setting for setting.
 *
 * The vocabulary of product types is not this component's: the caller reads it
 * from analytics and passes the codes it holds, so a type the screens cannot
 * offer is reported here and not decided here.
 */
class ORES_ORE_CORE_EXPORT pricing_engines_import {
public:
    /** The root element of the document ORE reads. */
    static constexpr std::string_view root_element = "PricingEngines";

    /**
     * @brief Reads the text of a pricing engines document.
     *
     * A text that is not well formed, or whose root element is another
     * document's, answers a result whose =refusal= names what was found. The
     * import never guesses: a partial reading of the wrong document would write
     * half a configuration.
     *
     * @param text The document.
     */
    static pricing_engines_import_file parse(std::string_view text);

    /**
     * @brief States how the file differs from the target.
     *
     * A target with no products and no parameters is a configuration the file
     * would create, so every row the file holds is an addition. A product is
     * matched by its type and by which product of that type it is, because a
     * document may name the same type twice; a setting is matched within its
     * product by its scope and its name, for the same reason.
     *
     * @param file The file, as parse read it.
     * @param target The configuration the file would be written into, or an
     *               empty document for a new configuration.
     * @param known_types The product types the vocabulary holds.
     */
    static pricing_engines_import_difference
    difference(const pricing_engines_import_file& file,
               const ores::analytics::messaging::pricing_engines_document& target,
               const std::set<std::string>& known_types);

    /**
     * @brief The removals a review keeps when the person changes nothing.
     *
     * A file that omits a row usually omits it by accident, so every removed row
     * is kept unless the person says otherwise. That decision is stated once,
     * here, so a screen and a shell command cannot default differently.
     *
     * @param difference The difference the person is reviewing.
     */
    static std::vector<std::string>
    kept_by_default(const pricing_engines_import_difference& difference);

    /**
     * @brief The document to save: the file's rows laid over the target's.
     *
     * A row the target already holds keeps its identity and version, so the
     * write corrects that row rather than replacing it with a new one, and the
     * history stays one line per setting. A kept removal is carried from the
     * target, with the settings it owns, because the file says nothing about
     * it. Everything else in the target is absent from the answer, which is how
     * the analytics save removes it.
     *
     * The header is the target's, or an empty one; the caller names the
     * configuration it writes.
     *
     * @param file The file, as parse read it.
     * @param target The configuration the file would be written into.
     * @param kept_removals The keys of the removed rows the person chose to
     *                      keep.
     */
    static ores::analytics::messaging::pricing_engines_document
    merge(const pricing_engines_import_file& file,
          const ores::analytics::messaging::pricing_engines_document& target,
          const std::vector<std::string>& kept_removals);
};

}

#endif
