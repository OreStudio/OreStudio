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
#ifndef ORES_UTILITY_CODEC_JSON_DOCUMENT_HPP
#define ORES_UTILITY_CODEC_JSON_DOCUMENT_HPP

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yyjson.h>

namespace ores::utility::codec {

class json_document;

/**
 * @brief A value of a document, seen without a type.
 *
 * The codec turns a stated type into JSON and back; a document with no type --
 * an OpenAPI spec a person posted, the arguments of a workflow step, the row an
 * export writes -- needs the other half of the same idea: a value a program
 * walks and builds member by member. This is a view of one value in a
 * document's tree, so it lives no longer than the document it came from.
 *
 * A member the document does not state reads as a value that does not exist,
 * rather than as an error, because most callers read what a producer chose to
 * send.
 */
class json_value {
public:
    json_value() = default;

    /// Whether this names a value at all.
    [[nodiscard]] bool exists() const {
        return val_ != nullptr;
    }

    /// The same in a condition, as an optional reads.
    explicit operator bool() const {
        return exists();
    }

    [[nodiscard]] bool is_object() const {
        return val_ != nullptr && yyjson_mut_is_obj(val_);
    }

    [[nodiscard]] bool is_array() const {
        return val_ != nullptr && yyjson_mut_is_arr(val_);
    }

    [[nodiscard]] bool is_string() const {
        return val_ != nullptr && yyjson_mut_is_str(val_);
    }

    [[nodiscard]] bool is_number() const {
        return val_ != nullptr && yyjson_mut_is_num(val_);
    }

    [[nodiscard]] bool is_bool() const {
        return val_ != nullptr && yyjson_mut_is_bool(val_);
    }

    /// The members of an object, or the elements of an array.
    [[nodiscard]] std::size_t size() const {
        if (is_object())
            return yyjson_mut_obj_size(val_);
        if (is_array())
            return yyjson_mut_arr_size(val_);
        return 0;
    }

    [[nodiscard]] bool has(std::string_view name) const {
        return member(name).exists();
    }

    /// The member an object states under the name, or nothing.
    [[nodiscard]] json_value member(std::string_view name) const {
        if (!is_object())
            return json_value{};
        const std::string key(name);
        return json_value{doc_, yyjson_mut_obj_get(val_, key.c_str())};
    }

    /// The element an array states at the position, or nothing.
    [[nodiscard]] json_value element(std::size_t index) const {
        if (!is_array() || index >= size())
            return json_value{};
        return json_value{doc_, yyjson_mut_arr_get(val_, index)};
    }

    /// The values of an array, in order.
    [[nodiscard]] std::vector<json_value> elements() const {
        std::vector<json_value> out;
        if (!is_array())
            return out;
        out.reserve(size());
        for (std::size_t i = 0; i < size(); ++i)
            out.push_back(element(i));
        return out;
    }

    /// The members of an object, as the document states them.
    [[nodiscard]] std::vector<std::pair<std::string, json_value>> members() const {
        std::vector<std::pair<std::string, json_value>> out;
        if (!is_object())
            return out;
        yyjson_mut_obj_iter iter;
        yyjson_mut_obj_iter_init(val_, &iter);
        while (yyjson_mut_val* key = yyjson_mut_obj_iter_next(&iter)) {
            const char* name = yyjson_mut_get_str(key);
            out.emplace_back(name != nullptr ? std::string(name) : std::string(),
                             json_value{doc_, yyjson_mut_obj_iter_get_val(key)});
        }
        return out;
    }

    /// The text a string states, or nothing for any other kind.
    [[nodiscard]] std::optional<std::string_view> text() const {
        if (!is_string())
            return std::nullopt;
        const char* text = yyjson_mut_get_str(val_);
        if (text == nullptr)
            return std::nullopt;
        return std::string_view(text, yyjson_mut_get_len(val_));
    }

    /// The text a string states, or the fallback for any other kind.
    [[nodiscard]] std::string text_or(std::string_view fallback = {}) const {
        const auto stated = text();
        return stated ? std::string(*stated) : std::string(fallback);
    }

    /// The whole number a number states, or nothing.
    [[nodiscard]] std::optional<std::int64_t> integer() const {
        if (!is_number())
            return std::nullopt;
        return yyjson_mut_get_sint(val_);
    }

    /// Whether a number is stated as a whole number rather than a float.
    [[nodiscard]] bool is_integer() const {
        return val_ != nullptr && yyjson_mut_is_int(val_);
    }

    /// The number a number states as a binary float, or nothing.
    [[nodiscard]] std::optional<double> real() const {
        if (!is_number())
            return std::nullopt;
        return yyjson_mut_get_num(val_);
    }

    /// The truth a boolean states, or nothing.
    [[nodiscard]] std::optional<bool> flag() const {
        if (!is_bool())
            return std::nullopt;
        return yyjson_mut_get_bool(val_);
    }

    /// The value as text, which is a document of its own.
    [[nodiscard]] std::string write() const {
        if (val_ == nullptr)
            return {};
        std::size_t length = 0;
        char* text = yyjson_mut_val_write(val_, 0, &length);
        if (text == nullptr)
            return {};
        std::string out(text, length);
        free(text);
        return out;
    }

    /// States a string member on an object, replacing one of the same name.
    void set(std::string_view name, std::string_view value) {
        set_value(name, yyjson_mut_strncpy(doc_, value.data(), value.size()));
    }

    /// States a string member from a literal, which a bool would otherwise take.
    void set(std::string_view name, const char* value) {
        set(name, std::string_view(value));
    }

    /// States a whole number member on an object.
    void set(std::string_view name, std::int64_t value) {
        set_value(name, yyjson_mut_int(doc_, value));
    }

    /// States a binary float member on an object.
    void set(std::string_view name, double value) {
        set_value(name, yyjson_mut_real(doc_, value));
    }

    /// States a member that holds no value, which a strict reader requires.
    void set_null(std::string_view name) {
        set_value(name, yyjson_mut_null(doc_));
    }

    /// States a truth member on an object.
    void set(std::string_view name, bool value) {
        set_value(name, yyjson_mut_bool(doc_, value));
    }

    /// Adds a member that holds a fresh object.
    json_value add_object(std::string_view name) {
        auto value = json_value{doc_, yyjson_mut_obj(doc_)};
        set_value(name, value.val_);
        return value;
    }

    /// Adds a member that holds a fresh array.
    json_value add_array(std::string_view name) {
        auto value = json_value{doc_, yyjson_mut_arr(doc_)};
        set_value(name, value.val_);
        return value;
    }

    /// Adds a member that copies a value, which may belong to another document.
    json_value add(std::string_view name, const json_value& value) {
        auto copy = json_value{doc_, yyjson_mut_val_mut_copy(doc_, value.val_)};
        set_value(name, copy.val_);
        return copy;
    }

    /// Appends a fresh object to an array.
    json_value push_object() {
        auto value = json_value{doc_, yyjson_mut_obj(doc_)};
        yyjson_mut_arr_append(val_, value.val_);
        return value;
    }

    /// Appends a fresh array to an array.
    json_value push_array() {
        auto value = json_value{doc_, yyjson_mut_arr(doc_)};
        yyjson_mut_arr_append(val_, value.val_);
        return value;
    }

    /// Appends a string to an array.
    void push(std::string_view value) {
        yyjson_mut_arr_append(val_, yyjson_mut_strncpy(doc_, value.data(), value.size()));
    }

    /// Appends a literal, which the bool overload would otherwise take.
    void push(const char* value) {
        push(std::string_view(value));
    }

    /// Appends a value that holds nothing.
    void push_null() {
        yyjson_mut_arr_append(val_, yyjson_mut_null(doc_));
    }

    /// Appends a truth to an array.
    void push(bool value) {
        yyjson_mut_arr_append(val_, yyjson_mut_bool(doc_, value));
    }

    /// Appends a binary float to an array.
    void push(double value) {
        yyjson_mut_arr_append(val_, yyjson_mut_real(doc_, value));
    }

    /// Appends a whole number to an array.
    void push(std::int64_t value) {
        yyjson_mut_arr_append(val_, yyjson_mut_int(doc_, value));
    }

    /// Appends a copy of a value, which may belong to another document.
    void push(const json_value& value) {
        yyjson_mut_arr_append(val_, yyjson_mut_val_mut_copy(doc_, value.val_));
    }

private:
    friend class json_document;

    json_value(yyjson_mut_doc* doc, yyjson_mut_val* val)
        : doc_(doc)
        , val_(val) {}

    void set_value(std::string_view name, yyjson_mut_val* value) {
        if (!is_object() || value == nullptr)
            return;
        yyjson_mut_obj_put(val_, yyjson_mut_strncpy(doc_, name.data(), name.size()), value);
    }

    yyjson_mut_doc* doc_{nullptr};
    yyjson_mut_val* val_{nullptr};
};

/**
 * @brief A JSON document a program reads and builds without a shape.
 *
 * It owns one document, so every value it hands out lives no longer than it
 * does. A document that was parsed accepts members and elements like one that
 * was built, because a reader that rewrites what it read is the common case:
 * the OpenAPI spec a person posts is the one the server serves.
 */
class json_document {
public:
    json_document() = default;

    ~json_document() {
        if (doc_ != nullptr)
            yyjson_mut_doc_free(doc_);
    }

    json_document(json_document&& other) noexcept
        : doc_(std::exchange(other.doc_, nullptr)) {}

    json_document& operator=(json_document&& other) noexcept {
        if (this != &other) {
            if (doc_ != nullptr)
                yyjson_mut_doc_free(doc_);
            doc_ = std::exchange(other.doc_, nullptr);
        }
        return *this;
    }

    json_document(const json_document&) = delete;
    json_document& operator=(const json_document&) = delete;

    /**
     * @brief The document the text states, or why it states none.
     */
    [[nodiscard]] static std::expected<json_document, std::string>
    parse(std::string_view text) noexcept {
        yyjson_doc* read = yyjson_read(text.data(), text.size(), 0);
        if (read == nullptr)
            return std::unexpected("Not JSON: " + std::string(text));
        yyjson_mut_doc* doc = yyjson_doc_mut_copy(read, nullptr);
        yyjson_doc_free(read);
        if (doc == nullptr)
            return std::unexpected("Could not read the JSON document");
        json_document out;
        out.doc_ = doc;
        return out;
    }

    /// A document whose root is an object.
    [[nodiscard]] static json_document make_object() {
        json_document out;
        out.doc_ = yyjson_mut_doc_new(nullptr);
        yyjson_mut_doc_set_root(out.doc_, yyjson_mut_obj(out.doc_));
        return out;
    }

    /// A document whose root is an array.
    [[nodiscard]] static json_document make_array() {
        json_document out;
        out.doc_ = yyjson_mut_doc_new(nullptr);
        yyjson_mut_doc_set_root(out.doc_, yyjson_mut_arr(out.doc_));
        return out;
    }

    /// Whether this holds a document at all.
    [[nodiscard]] bool exists() const {
        return doc_ != nullptr;
    }

    /// The root value.
    [[nodiscard]] json_value root() const {
        if (doc_ == nullptr)
            return json_value{};
        return json_value{doc_, yyjson_mut_doc_get_root(doc_)};
    }

    /// The document as text.
    [[nodiscard]] std::string write() const {
        if (doc_ == nullptr)
            return {};
        char* text = yyjson_mut_write(doc_, 0, nullptr);
        if (text == nullptr)
            return {};
        std::string out(text);
        free(text);
        return out;
    }

private:
    yyjson_mut_doc* doc_{nullptr};
};

}

#endif
