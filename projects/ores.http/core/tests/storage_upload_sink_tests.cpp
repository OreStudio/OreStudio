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
#include "ores.http.core/routes/storage_upload_sink.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <string>

using namespace ores::http::server::routes;
using namespace ores::logging;
namespace fs = std::filesystem;

namespace {

/**
 * @brief A directory that removes itself, so a test leaves no files behind.
 */
class temporary_root final {
public:
    temporary_root() {
        const auto stamp = std::chrono::steady_clock::now().time_since_epoch().count();
        path_ = fs::temp_directory_path() / ("ores-upload-sink-" + std::to_string(stamp));
    }

    ~temporary_root() {
        std::error_code ec;
        fs::remove_all(path_, ec);
    }

    temporary_root(const temporary_root&) = delete;
    temporary_root& operator=(const temporary_root&) = delete;

    [[nodiscard]] const fs::path& path() const {
        return path_;
    }

private:
    fs::path path_;
};

/**
 * @brief How many files under a root are part files waiting to be published.
 */
std::size_t count_parts(const fs::path& root) {
    auto parts = std::size_t{0};
    if (!fs::exists(root))
        return parts;
    for (const auto& entry : fs::recursive_directory_iterator(root))
        if (entry.path().extension() == ".part")
            ++parts;
    return parts;
}

}

TEST_CASE("an_upload_is_written_to_a_part_file_until_it_is_published", "[storage_upload_sink]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an object appears only when it is published";

    temporary_root root;
    ores::storage::filesystem::local_store store(root.path());
    const auto content = std::string(3 * 1024, 'x');

    {
        storage_upload_sink sink(store, "bucket", "key");
        for (auto offset = std::size_t{0}; offset < content.size(); offset += 1024)
            REQUIRE(sink.consume(content.substr(offset, 1024)));
        REQUIRE(sink.finish());

        // Nothing is at the key yet, and the bytes are already on disk.
        REQUIRE(!store.exists("bucket", "key"));
        REQUIRE(count_parts(root.path()) == 1);

        REQUIRE(sink.publish());
    }

    REQUIRE(store.exists("bucket", "key"));
    REQUIRE(count_parts(root.path()) == 0);
    REQUIRE(store.get("bucket", "key") == content);
}

TEST_CASE("an_upload_that_is_not_published_leaves_nothing", "[storage_upload_sink]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an abandoned upload leaves no object and no part";

    temporary_root root;
    ores::storage::filesystem::local_store store(root.path());

    {
        storage_upload_sink sink(store, "bucket", "key");
        REQUIRE(sink.consume(std::string(2048, 'x')));
        REQUIRE(sink.finish());
        // The session aborts a sink that was not published.
        sink.abort();
    }

    REQUIRE(!store.exists("bucket", "key"));
    REQUIRE(count_parts(root.path()) == 0);
}

TEST_CASE("two_uploads_of_one_key_never_share_bytes", "[storage_upload_sink]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that one upload cannot publish another's half-written file";

    temporary_root root;
    ores::storage::filesystem::local_store store(root.path());

    storage_upload_sink first(store, "bucket", "key");
    storage_upload_sink second(store, "bucket", "key");

    REQUIRE(first.consume("aaa"));
    REQUIRE(second.consume("bbb"));
    REQUIRE(first.finish());
    REQUIRE(second.finish());
    REQUIRE(first.publish());
    REQUIRE(second.publish());

    /*
     * One of the two bodies, whole. Sharing a part file would interleave them,
     * and the reader would see "aaabbb" or a mixture of the two.
     */
    const auto stored = store.get("bucket", "key");
    REQUIRE((stored == "aaa" || stored == "bbb"));
    REQUIRE(count_parts(root.path()) == 0);
}

TEST_CASE("a_part_file_is_not_listed_as_an_object", "[storage_upload_sink]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a listing shows objects and not uploads in flight";

    temporary_root root;
    ores::storage::filesystem::local_store store(root.path());

    storage_upload_sink sink(store, "bucket", "key");
    REQUIRE(sink.consume("the bytes so far"));
    REQUIRE(sink.finish());

    REQUIRE(store.list("bucket", "").empty());

    REQUIRE(sink.publish());
    const auto entries = store.list("bucket", "");
    REQUIRE(entries.size() == 1);
    REQUIRE(entries.front().key == "key");
}

TEST_CASE("an_upload_reports_the_digest_of_what_it_took", "[storage_upload_sink]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the digest is of the bytes, not of the file name";

    temporary_root root;
    ores::storage::filesystem::local_store store(root.path());
    const auto content = std::string("the bytes an engine package carries");

    storage_upload_sink sink(store, "bucket", "key");
    REQUIRE(sink.consume(content));
    REQUIRE(sink.finish());
    REQUIRE(sink.publish());

    REQUIRE(sink.byte_count() == content.size());
    REQUIRE(sink.digest() == ores::utility::crypto::sha256::hex_digest(content));
    REQUIRE(store.get("bucket", "key") == content);
}

TEST_CASE("an_upload_to_an_invalid_key_is_refused_before_anything_is_written",
          "[storage_upload_sink]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a key outside the bucket is not a place to write";

    temporary_root root;
    ores::storage::filesystem::local_store store(root.path());

    REQUIRE_THROWS_AS(storage_upload_sink(store, "bucket", "../escape"), std::invalid_argument);
}
