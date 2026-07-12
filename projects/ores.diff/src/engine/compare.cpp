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
#include "ores.diff/engine/compare.hpp"
#include <algorithm>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ores::diff::engine {

namespace {

using ores::diff::domain::diff_span;

[[nodiscard]] bool is_utf8_continuation_byte(char c) {
    return (static_cast<unsigned char>(c) & 0xC0) == 0x80;
}

/**
 * @brief Common-prefix/suffix diff between two single-line values,
 * appending the differing byte ranges to a_spans/b_spans (offset by
 * a_base/b_base, so callers embedding a line within a larger string
 * get whole-string offsets). Byte boundaries are trimmed so a span
 * never splits a UTF-8 multi-byte sequence. No-op when a == b.
 */
void diff_line_into(std::string_view a,
                    std::string_view b,
                    std::size_t a_base,
                    std::size_t b_base,
                    std::vector<diff_span>& a_spans,
                    std::vector<diff_span>& b_spans) {
    if (a == b)
        return;

    const std::size_t max_common = std::min(a.size(), b.size());

    std::size_t prefix_len = 0;
    while (prefix_len < max_common && a[prefix_len] == b[prefix_len])
        ++prefix_len;

    const std::size_t max_suffix = max_common - prefix_len;
    std::size_t suffix_len = 0;
    while (suffix_len < max_suffix && a[a.size() - 1 - suffix_len] == b[b.size() - 1 - suffix_len])
        ++suffix_len;

    // Trim the prefix/suffix boundaries so neither splits a UTF-8
    // multi-byte sequence. a and b are identical over [0, prefix_len)
    // and over their respective suffix ranges, so checking either
    // side is equivalent.
    while (prefix_len > 0 && prefix_len < a.size() && is_utf8_continuation_byte(a[prefix_len]))
        --prefix_len;
    while (suffix_len > 0 && is_utf8_continuation_byte(a[a.size() - suffix_len]))
        --suffix_len;

    const std::size_t a_mid_start = prefix_len;
    const std::size_t a_mid_end = a.size() - suffix_len;
    if (a_mid_end > a_mid_start)
        a_spans.push_back({.offset = a_base + a_mid_start, .length = a_mid_end - a_mid_start});

    const std::size_t b_mid_start = prefix_len;
    const std::size_t b_mid_end = b.size() - suffix_len;
    if (b_mid_end > b_mid_start)
        b_spans.push_back({.offset = b_base + b_mid_start, .length = b_mid_end - b_mid_start});
}

/**
 * @brief One line of a multiline value: its content and its byte
 * offset within the whole string (excluding the separating '\n').
 */
struct offset_line final {
    std::size_t offset{};
    std::string_view content;
};

[[nodiscard]] std::vector<offset_line> split_lines_with_offsets(std::string_view s) {
    std::vector<offset_line> lines;
    std::size_t start = 0;
    while (true) {
        const auto nl = s.find('\n', start);
        if (nl == std::string_view::npos) {
            lines.push_back({.offset = start, .content = s.substr(start)});
            break;
        }
        lines.push_back({.offset = start, .content = s.substr(start, nl - start)});
        start = nl + 1;
    }
    return lines;
}

enum class line_op { equal, remove, insert };

struct line_edit final {
    line_op op{};
    std::size_t old_index{};
    std::size_t new_index{};
};

/**
 * @brief Line-level LCS edit script between two line sequences.
 */
[[nodiscard]] std::vector<line_edit> lcs_line_diff(const std::vector<offset_line>& old_lines,
                                                   const std::vector<offset_line>& new_lines) {
    const std::size_t n = old_lines.size();
    const std::size_t m = new_lines.size();

    std::vector<std::vector<int>> dp(n + 1, std::vector<int>(m + 1, 0));
    for (std::size_t i = 1; i <= n; ++i) {
        for (std::size_t j = 1; j <= m; ++j) {
            if (old_lines[i - 1].content == new_lines[j - 1].content)
                dp[i][j] = dp[i - 1][j - 1] + 1;
            else
                dp[i][j] = std::max(dp[i - 1][j], dp[i][j - 1]);
        }
    }

    std::vector<line_edit> ops;
    std::size_t i = n;
    std::size_t j = m;
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && old_lines[i - 1].content == new_lines[j - 1].content) {
            ops.push_back({.op = line_op::equal, .old_index = i - 1, .new_index = j - 1});
            --i;
            --j;
        } else if (j > 0 && (i == 0 || dp[i][j - 1] >= dp[i - 1][j])) {
            ops.push_back({.op = line_op::insert, .old_index = 0, .new_index = j - 1});
            --j;
        } else {
            ops.push_back({.op = line_op::remove, .old_index = i - 1, .new_index = 0});
            --i;
        }
    }
    std::ranges::reverse(ops);
    return ops;
}

/**
 * @brief Diffs two multiline values line by line: unchanged lines
 * contribute no spans; a contiguous run of removed/inserted lines
 * between two matched anchors is paired positionally and each pair
 * gets a per-line token (prefix/suffix) diff; unpaired leftover
 * lines in the longer run are marked as fully changed.
 */
void diff_multiline_into(std::string_view a,
                         std::string_view b,
                         std::vector<diff_span>& a_spans,
                         std::vector<diff_span>& b_spans) {
    const auto old_lines = split_lines_with_offsets(a);
    const auto new_lines = split_lines_with_offsets(b);
    const auto ops = lcs_line_diff(old_lines, new_lines);

    std::vector<std::size_t> removed_run;
    std::vector<std::size_t> inserted_run;

    const auto flush_run = [&]() {
        const std::size_t paired = std::min(removed_run.size(), inserted_run.size());
        for (std::size_t k = 0; k < paired; ++k) {
            const auto& old_line = old_lines[removed_run[k]];
            const auto& new_line = new_lines[inserted_run[k]];
            diff_line_into(old_line.content,
                           new_line.content,
                           old_line.offset,
                           new_line.offset,
                           a_spans,
                           b_spans);
        }
        for (std::size_t k = paired; k < removed_run.size(); ++k) {
            const auto& old_line = old_lines[removed_run[k]];
            if (!old_line.content.empty())
                a_spans.push_back({.offset = old_line.offset, .length = old_line.content.size()});
        }
        for (std::size_t k = paired; k < inserted_run.size(); ++k) {
            const auto& new_line = new_lines[inserted_run[k]];
            if (!new_line.content.empty())
                b_spans.push_back({.offset = new_line.offset, .length = new_line.content.size()});
        }
        removed_run.clear();
        inserted_run.clear();
    };

    for (const auto& op : ops) {
        switch (op.op) {
            case line_op::equal:
                flush_run();
                break;
            case line_op::remove:
                removed_run.push_back(op.old_index);
                break;
            case line_op::insert:
                inserted_run.push_back(op.new_index);
                break;
        }
    }
    flush_run();
}

void compute_spans(domain::diff_entry& entry) {
    const bool multiline = entry.old_value.find('\n') != std::string::npos ||
                           entry.new_value.find('\n') != std::string::npos;
    if (multiline)
        diff_multiline_into(entry.old_value, entry.new_value, entry.old_spans, entry.new_spans);
    else
        diff_line_into(entry.old_value, entry.new_value, 0, 0, entry.old_spans, entry.new_spans);
}

}

domain::diff_result compute(const std::vector<domain::field_value>& previous,
                            const std::vector<domain::field_value>& current) {
    // Index the previous fields by name. First occurrence wins when a
    // name repeats; string_view keys borrow from `previous`, which
    // outlives the map.
    std::unordered_map<std::string_view, const domain::field_value*> by_name;
    by_name.reserve(previous.size());
    for (const auto& field : previous)
        by_name.try_emplace(field.name, &field);

    domain::diff_result result;

    // Current-list order drives the output: changed and added fields.
    std::unordered_set<std::string_view> seen_in_current;
    seen_in_current.reserve(current.size());
    for (const auto& field : current) {
        if (!seen_in_current.insert(field.name).second)
            continue; // First occurrence wins.

        const auto it = by_name.find(field.name);
        if (it == by_name.end()) {
            // Added: absent side is empty.
            result.entries.push_back(
                {.field_name = field.name, .old_value = {}, .new_value = field.value});
        } else if (it->second->value != field.value) {
            result.entries.push_back({.field_name = field.name,
                                      .old_value = it->second->value,
                                      .new_value = field.value});
        }
    }

    // Removed fields follow, in the previous list's order.
    std::unordered_set<std::string_view> seen_in_previous;
    seen_in_previous.reserve(previous.size());
    for (const auto& field : previous) {
        if (!seen_in_previous.insert(field.name).second)
            continue; // First occurrence wins.

        if (!seen_in_current.contains(field.name)) {
            result.entries.push_back(
                {.field_name = field.name, .old_value = field.value, .new_value = {}});
        }
    }

    for (auto& entry : result.entries)
        compute_spans(entry);

    return result;
}

}
