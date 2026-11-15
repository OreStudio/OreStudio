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
#ifndef ORES_HTTP_CORE_ROUTES_ORE_VALUE_SETS_HPP
#define ORES_HTTP_CORE_ROUTES_ORE_VALUE_SETS_HPP

#include "ores.http.core/export.hpp"
#include <functional>
#include <map>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes {

/**
 * @brief One choice a control offers: the value a configuration holds, and the
 * words a person reads.
 */
struct value_choice final {
    std::string value;
    std::string label;
};

/**
 * @brief The allowed values of one ORE parameter, as the value set tables state
 * them.
 */
struct value_fields final {
    /**
     * @brief The code of the set, or empty when the model holds none.
     */
    std::string value_set;

    /**
     * @brief The kind one value may be, in the words of the table, or empty when
     * the model holds no set.
     */
    std::string source_kind;

    /**
     * @brief The catalogue a set draws on, by the code the catalogue table gives
     * it, or empty when it draws on none.
     */
    std::string catalogue;

    /**
     * @brief The values a list offers, which is none for the kinds that take a
     * value.
     */
    std::vector<value_choice> choices;

    std::optional<double> minimum;
    std::optional<double> maximum;

    /**
     * @brief The expression a regex set states, or empty.
     */
    std::string pattern;
};

/**
 * @brief One value set and the rule its values obey.
 */
struct value_set_row final {
    std::string code;
    std::string source_kind;
    std::string catalogue_code;
    std::optional<double> minimum;
    std::optional<double> maximum;
    std::string pattern;
};

/**
 * @brief One value of an enumerated set.
 */
struct value_set_value_row final {
    std::string value_set_code;
    std::string value;
    std::string label;
    int display_order = 0;

    /**
     * @brief The canonical form this row is another rendering of, or empty when
     * the row is a choice of its own.
     */
    std::string canonical_value;
};

/**
 * @brief The relation a catalogue set draws its values from.
 */
struct value_set_catalogue_row final {
    std::string code;
    std::string relation;
};

/**
 * @brief The value set one setting of one scope names.
 */
struct setting_value_set_row final {
    std::string scope;
    std::string name;
    std::string value_set_code;
};

/**
 * @brief The four value set tables, as the service returned them.
 */
struct value_set_tables final {
    std::vector<value_set_row> sets;
    std::vector<value_set_value_row> values;
    std::vector<value_set_catalogue_row> catalogues;
    std::vector<setting_value_set_row> links;
};

/**
 * @brief A value set the data states in a way this build cannot read.
 *
 * The sentence names the set and the word, because the person who can fix it is
 * reading the log rather than the screen.
 */
class ORES_HTTP_CORE_EXPORT value_set_error final : public std::runtime_error {
public:
    explicit value_set_error(const std::string& sentence);
};

/**
 * @brief The kinds a value set states, in the words the table uses.
 *
 * A set states one of them, so a word outside this list is a set the model does
 * not describe and a refusal rather than a guess. The empty word is a setting
 * the model holds no set for, and it is not one of these.
 */
[[nodiscard]] ORES_HTTP_CORE_EXPORT std::span<const std::string_view> source_kinds();

/**
 * @brief Reads the value set tables once and answers the allowed values of a
 * setting by its scope and its name.
 *
 * The four tables are read by the caller, which owns the session, and handed
 * here as rows. A catalogue set names a relation, and the choices of a relation
 * come from the caller too, because the relation is another component's
 * collection: a reader here would have to know every component that holds one.
 * A relation is read once however many sets draw on it, and not at all until a
 * set that draws on it is asked for.
 *
 * Every kind states its own answer, so a kind this build does not know, and a
 * setting the model holds no set for, are refusals a person reads rather than a
 * control that checks nothing.
 */
class ORES_HTTP_CORE_EXPORT value_set_reader final {
public:
    /**
     * @brief The choices of one catalogue relation, as the caller reads them.
     *
     * Throws value_set_error when this build knows no way to read the relation.
     */
    using relation_reader = std::function<std::vector<value_choice>(std::string_view relation)>;

    /**
     * @param tables The four value set tables, as the service returned them.
     * @param catalogue_of The choices of one relation, read on first use.
     */
    value_set_reader(value_set_tables tables, relation_reader catalogue_of);

    /**
     * @brief The allowed values of the setting with this scope and name.
     *
     * @throws value_set_error when a set the model states cannot be read.
     */
    [[nodiscard]] value_fields fields_of(std::string_view scope, std::string_view name) const;

private:
    [[nodiscard]] std::vector<value_choice> choices_of(std::string_view code,
                                                       const std::string& kind) const;

    [[nodiscard]] const std::vector<value_choice>&
    relation_choices(std::string_view code, std::string_view relation) const;

    relation_reader catalogue_of_;
    std::map<std::string, std::string> set_for_setting_;
    std::map<std::string, value_set_row> set_of_;
    std::map<std::string, std::string> relation_of_;
    std::map<std::string, std::vector<value_choice>> enumerated_;
    mutable std::map<std::string, std::vector<value_choice>> by_relation_;
    mutable std::map<std::string, value_fields> answered_;
};

}

#endif
