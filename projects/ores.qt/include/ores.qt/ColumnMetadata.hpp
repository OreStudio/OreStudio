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
#ifndef ORES_QT_COLUMN_METADATA_HPP
#define ORES_QT_COLUMN_METADATA_HPP

#include <array>
#include <cstddef>
#include <string_view>
#include <vector>
#include <QVector>

namespace ores::qt {

/**
 * @brief Sentinel value for column default width meaning "auto-size to contents".
 *
 * Used in ColumnMetadata::default_width to indicate that the column should
 * be sized using QHeaderView::ResizeToContents rather than a fixed pixel width.
 */
inline constexpr int kColumnWidthAuto = -1;

/**
 * @brief Column rendering style for table delegates.
 */
enum class column_style {
    text_left,          ///< Proportional font, left-aligned (default).
    text_center,        ///< Proportional font, centered.
    mono_left,          ///< Monospace, left-aligned.
    mono_center,        ///< Monospace, centered.
    mono_bold_left,     ///< Monospace bold, left-aligned.
    mono_right,         ///< Monospace, right-aligned.
    mono_bold_center,   ///< Monospace bold, centered.
    icon_centered,      ///< Centered DecorationRole icon (flag columns).
    badge_centered      ///< Coloured badge pill, centered.
};

/**
 * @brief Metadata for a table column.
 *
 * Provides a single source of truth for column configuration including
 * header text, rendering style, visibility, and width. Used by models
 * to define columns in a way that automatically stays in sync with
 * delegate styles and column visibility settings.
 */
struct ColumnMetadata {
    /** Column index (should match the model's Column enum value). */
    int column;
    /** Header text displayed in the table view. */
    std::string_view header;
    /** Rendering style for the column (font, alignment). */
    column_style style;
    /** If true, column is hidden by default. */
    bool hidden_by_default;
    /** Default width in pixels, or kColumnWidthAuto for auto-size. */
    int default_width;
};

/**
 * @brief Helper to build a std::array of column styles from metadata.
 *
 * Usage in model header:
 *   static constexpr std::array<column_style, kColumnCount> kStylesArray = makeStylesArray(kColumns);
 *
 * @tparam N The size of the metadata array.
 * @param columns The column metadata array.
 * @return std::array of column_style values.
 */
template <std::size_t N>
[[nodiscard]] static constexpr auto makeStylesArray(const std::array<ColumnMetadata, N>& columns) {
    std::array<column_style, N> styles{};
    for (std::size_t i = 0; i < N; ++i) {
        styles[i] = columns[i].style;
    }
    return styles;
}

/**
 * @brief Helper to build a vector of column styles (runtime, cached).
 *
 * Note: Due to C++ static initialization limitations, models should inline this:
 *   static std::vector<column_style> const& columnStyles() {
 *       static std::vector<column_style> const kStylesVector = []() {
 *           std::vector<column_style> result;
 *           result.reserve(kColumnCount);
 *           for (std::size_t i = 0; i < kColumnCount; ++i)
 *               result.push_back(kColumns[i].style);
 *           return result;
 *       }();
 *       return kStylesVector;
 *   }
 *
 * This template is provided for reference but cannot be used directly due to
 * lambda capture limitations with function parameters in static initialization.
 *
 * @tparam N The size of the metadata array.
 * @param columns The column metadata array.
 * @return Static const vector of column styles.
 */
template <std::size_t N>
[[nodiscard]] static std::vector<column_style>
columnStylesVector(const std::array<ColumnMetadata, N>& columns) {
    std::vector<column_style> result;
    result.reserve(N);
    for (std::size_t i = 0; i < N; ++i) {
        result.push_back(columns[i].style);
    }
    return result;
}

/**
 * @brief Helper to build an array of hidden column indices at compile-time.
 *
 * Note: Due to C++ constexpr limitations, the count must be computed inline.
 * Usage in model header:
 *   static constexpr std::size_t kHiddenCount = []() constexpr {
 *       std::size_t c = 0;
 *       for (auto const& col : kColumns) if (col.hidden_by_default) ++c;
 *       return c;
 *   }();
 *   static constexpr std::array<int, kHiddenCount> kHiddenArray = []() constexpr {
 *       std::array<int, kHiddenCount> h{};
 *       std::size_t idx = 0;
 *       for (std::size_t i = 0; i < kColumnCount; ++i)
 *           if (kColumns[i].hidden_by_default) h[idx++] = i;
 *       return h;
 *   }();
 *
 * @tparam N The size of the metadata array.
 * @param columns The column metadata array.
 * @return std::array of column indices that are hidden by default.
 */
template <std::size_t N>
[[nodiscard]] static constexpr auto
makeHiddenColumnsArray(const std::array<ColumnMetadata, N>& columns) {
    // Note: This can't be used directly in template parameters due to C++
    // constexpr limitations. Models should compute kHiddenCount inline instead.
    std::size_t count = 0;
    for (std::size_t i = 0; i < N; ++i) {
        if (columns[i].hidden_by_default) ++count;
    }
    std::array<int, 10> hidden{};  // Placeholder - don't use this function directly
    return hidden;
}

/**
 * @brief Helper to build a QVector of hidden column indices (runtime, cached).
 *
 * Usage in model header:
 *   static QVector<int> defaultHiddenColumns() {
 *       return ::ores::qt::defaultHiddenColumns(kColumns);
 *   }
 *
 * @tparam N The size of the metadata array.
 * @param columns The column metadata array.
 * @return Static const QVector of hidden column indices.
 */
template <std::size_t N>
[[nodiscard]] static QVector<int>
defaultHiddenColumns(const std::array<ColumnMetadata, N>& columns) {
    // Compute hidden count at runtime for the static init
    std::size_t count = 0;
    for (std::size_t i = 0; i < N; ++i) {
        if (columns[i].hidden_by_default) ++count;
    }

    static std::vector<int> const kHiddenVector = [&]{
        std::vector<int> result;
        result.reserve(count);
        for (std::size_t i = 0; i < N; ++i) {
            if (columns[i].hidden_by_default)
                result.push_back(static_cast<int>(i));
        }
        return result;
    }();

    return {kHiddenVector.begin(), kHiddenVector.end()};
}

}

#endif
