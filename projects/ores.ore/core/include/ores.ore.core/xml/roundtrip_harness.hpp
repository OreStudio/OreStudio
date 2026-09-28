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
#ifndef ORES_ORE_CORE_XML_ROUNDTRIP_HARNESS_HPP
#define ORES_ORE_CORE_XML_ROUNDTRIP_HARNESS_HPP

#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <filesystem>
#include <fstream>
#include <functional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief What one document's round trip did.
 *
 * A pass says the exported document equals the imported one as a parsed
 * document. A failure names the first difference, because a file that imported
 * and exported without throwing has only proved that it did not throw, not
 * that anything survived.
 */
struct roundtrip_outcome {
    bool passed = false;
    std::string detail;
};

/**
 * @brief Compares two parsed documents, naming the first difference or
 * returning empty when they agree.
 *
 * A kind supplies this because equality as parsed documents is not text
 * equality. The binding holds some numbers as text, and the mapper writes them
 * back in its own spelling, so ORE's =t0="0.0"= and our =t0="0"= are the same
 * bound. A generic text comparison would fail on that pair; a rule that
 * ignored text would hide a lost value. Only the kind can say which
 * differences are normalisation and which are loss.
 *
 * The path is handed over so the message can name the file that disagreed.
 */
template <typename Document>
using document_comparison = std::string (*)(const Document&, const Document&, const std::string&);

/**
 * @brief One document kind the harness can prove.
 *
 * The harness owns the walk, the count and the failure list. A kind owns its
 * mapper pair and its comparison, so a new kind is a value here rather than an
 * edit to the walk.
 */
struct roundtrip_kind {
    std::string name;
    std::string file_prefix;
    std::function<roundtrip_outcome(const std::filesystem::path&)> check;
};

/**
 * @brief The first place two pieces of text differ, or empty when they do not.
 *
 * Names the byte, the line and a window around both sides, so a difference a
 * generic comparison can see is a thing a reader can find.
 */
ORES_ORE_CORE_EXPORT std::string first_difference(const std::string& lhs, const std::string& rhs);

/**
 * @brief Compares two parsed documents by the canonical form of each.
 *
 * The comparison for a kind whose mapper preserves every value in the spelling
 * it was written in. A kind whose mapper normalises a field, because the
 * binding holds it as text and the type holds it as a number, supplies its own
 * comparison instead, so that the normalisation is stated rather than
 * discovered as a failure.
 */
template <typename Document>
std::string parsed_text_difference(const Document& lhs,
                                   const Document& rhs,
                                   const std::string& path) {
    const std::string left = save_data(lhs);
    const std::string right = save_data(rhs);
    if (left == right)
        return {};
    return path + ": " + first_difference(left, right);
}

/**
 * @brief Builds a kind from a mapper pair and a comparison.
 *
 * Import loads the file into the ORE binding, maps it to the entities, maps
 * them back and exports. The comparison then decides whether the export is the
 * same document as the import, so a mapper is free to normalise a document and
 * is not free to lose a field.
 */
template <typename Document, typename Mapped>
roundtrip_kind make_roundtrip_kind(std::string name,
                                   std::string file_prefix,
                                   Mapped (*map)(const Document&),
                                   Document (*reverse)(const Mapped&),
                                   document_comparison<Document> compare) {
    roundtrip_kind kind;
    kind.name = std::move(name);
    kind.file_prefix = std::move(file_prefix);
    kind.check = [map, reverse, compare](const std::filesystem::path& path) -> roundtrip_outcome {
        std::ifstream in(path, std::ios::binary);
        if (!in)
            return {false, path.string() + ": cannot be read"};
        std::ostringstream buffer;
        buffer << in.rdbuf();
        const std::string content = buffer.str();

        try {
            Document original;
            load_data(content, original);

            const Mapped mapped = map(original);
            const Document rebuilt = reverse(mapped);

            Document exported;
            load_data(save_data(rebuilt), exported);

            const std::string detail = compare(original, exported, path.string());
            if (detail.empty())
                return {true, {}};
            return {false, detail};
        } catch (const std::exception& e) {
            return {false, path.string() + ": " + e.what()};
        }
    };
    return kind;
}

/**
 * @brief Every file of @p file_prefix under @p corpus_root, in sorted order.
 *
 * The corpus of a kind is the set of files whose stem starts with its prefix,
 * which is what the census globs match. Exposed because a kind sometimes needs
 * to measure something other than a round trip, and whoever measures it should
 * not have to restate what its corpus is.
 */
ORES_ORE_CORE_EXPORT std::vector<std::filesystem::path>
files_of_kind(const std::string& file_prefix, const std::filesystem::path& corpus_root);

/**
 * @brief What a walk over one kind's corpus did.
 */
struct roundtrip_walk {
    std::string kind;
    int files = 0;
    int passed = 0;
    std::vector<std::string> failures;
};

/**
 * @brief Round trips every file of @p kind under @p corpus_root.
 *
 * The kind's corpus is the set of files whose stem starts with the kind's
 * prefix, which is what the census globs match. Files are visited in sorted
 * order so a failure list is stable across machines.
 */
ORES_ORE_CORE_EXPORT roundtrip_walk walk_kind(const roundtrip_kind& kind,
                                              const std::filesystem::path& corpus_root);

}

#endif
