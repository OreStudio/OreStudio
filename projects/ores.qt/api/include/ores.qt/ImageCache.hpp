/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_IMAGE_CACHE_HPP
#define ORES_QT_IMAGE_CACHE_HPP

#include "ores.assets.api/domain/image.hpp"
#include "ores.assets.api/messaging/assets_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/export.hpp"
#include <QFutureWatcher>
#include <QIcon>
#include <QObject>
#include <QPixmap>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ores::qt {

/**
 * @brief Cache for dynamically loaded images (flags, icons) from the server.
 *
 * This class manages the fetching and caching of images from the server.
 * Images are cached by their UUID (image_id) and can be retrieved for any
 * entity that references them.
 *
 * Simplified design:
 * - Single mapping: image_id to raw image data (SVG, JPEG, ...) and rendered QIcon
 * - Entities (currencies, countries) have their own image_id field
 * - Call getIcon(image_id) to get the icon for any image
 * - On-demand loading: if image not cached, loads from server
 *
 * Typical usage:
 * 1. Call loadAll() to preload images for current entities
 * 2. Use getIcon(image_id) to retrieve icons - loads on-demand if missing
 */
class ORES_QT_API ImageCache final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.image_cache";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ImageCache(ClientManager* clientManager, QObject* parent = nullptr);
    ~ImageCache() override = default;

    /**
     * @brief Preload images for current currencies and countries.
     *
     * Fetches current entities to discover their image_ids, then loads
     * those images into the cache. After completion, allLoaded() signal
     * is emitted.
     */
    void loadAll();

    /**
     * @brief Clear all caches and reload images.
     *
     * Use this after data changes (e.g., publishing datasets) to refresh
     * the cache with current server data. Emits allLoaded() when complete.
     */
    void reload();

    /**
     * @brief Get icon for an image by its UUID.
     *
     * If the image is cached, returns it immediately.
     * If not cached, triggers async load and returns placeholder icon.
     * When the image loads, imageLoaded(image_id) signal is emitted.
     *
     * @param image_id The image UUID as a string
     * @return QIcon for the image, or placeholder if not yet loaded
     */
    QIcon getIcon(const std::string& image_id);

    /**
     * @brief Check if an image is cached.
     *
     * @param image_id The image UUID
     * @return true if icon is cached, false otherwise
     */
    bool hasIcon(const std::string& image_id) const;

    /**
     * @brief Get the number of cached images.
     */
    std::size_t cachedIconCount() const {
        return image_icons_.size();
    }

    /**
     * @brief Check if images are currently being loaded.
     */
    bool isLoading() const {
        return is_loading_images_;
    }

    /**
     * @brief Clear all caches and reset load state.
     *
     * Call this before reload() when data has changed significantly
     * (e.g., after publishing to a different tenant or after major data
     * changes). This ensures the next reload() does a full refresh
     * instead of an incremental update.
     */
    void clear();

    /**
     * @brief Load list of all available images from the server.
     *
     * Fetches metadata for all images (without SVG data).
     * After completion, imageListLoaded() signal is emitted.
     */
    void loadImageList();

    /**
     * @brief Get the list of available images.
     *
     * @return Vector of image metadata (id, key, description)
     */
    const std::vector<assets::messaging::image_info>& availableImages() const {
        return available_images_;
    }

    /**
     * @brief Check if image list has been loaded.
     */
    bool hasImageList() const {
        return !available_images_.empty();
    }

    /**
     * @brief Load all available images from the image list.
     *
     * Fetches SVG data for all images in the available_images_ list.
     * After completion, allAvailableImagesLoaded() signal is emitted.
     */
    void loadAllAvailableImages();

    /**
     * @brief Set or remove a currency's image association.
     *
     * @param iso_code The currency ISO code
     * @param image_id The image ID to assign (empty to remove)
     * @param assigned_by Username performing the assignment
     */
    void setCurrencyImage(const std::string& iso_code,
                          const std::string& image_id,
                          const std::string& assigned_by);

    /**
     * @brief Set or remove a country's image association.
     *
     * @param alpha2_code The country alpha-2 code
     * @param image_id The image ID to assign (empty to remove)
     * @param assigned_by Username performing the assignment
     */
    void setCountryImage(const std::string& alpha2_code,
                         const std::string& image_id,
                         const std::string& assigned_by);

    /**
     * @brief Get the image ID for the "no-flag" placeholder.
     *
     * @return The image ID for the "no-flag" image, or empty string if not found
     */
    std::string getNoFlagImageId() const;

    /**
     * @brief Get the icon for the "no-flag" placeholder.
     *
     * @return The QIcon for the "no-flag" image, or empty icon if not loaded
     */
    QIcon getNoFlagIcon() const;

    /**
     * @brief Get flag icon for a currency by its ISO code.
     *
     * Looks up the currency's image_id from the cached mapping, then returns
     * the corresponding icon. Returns empty icon if not found.
     */
    QIcon getCurrencyFlagIcon(const std::string& iso_code);

    /**
     * @brief Get a crisp flag pixmap for a currency, rendered from SVG.
     *
     * Unlike getCurrencyFlagIcon(), which returns a QIcon backed by a small
     * raster ladder, this renders the cached SVG directly at the requested
     * height so it stays sharp at large sizes (e.g. hero headers). Returns an
     * empty pixmap if the currency has no image or the SVG is not yet cached.
     *
     * @param iso_code The currency ISO code
     * @param height   Target pixmap height in device pixels
     */
    QPixmap getCurrencyFlagPixmap(const std::string& iso_code, int height);

    /**
     * @brief Get flag icon for a country by its alpha-2 code.
     *
     * Looks up the country's image_id from the cached mapping, then returns
     * the corresponding icon. Returns empty icon if not found.
     */
    QIcon getCountryFlagIcon(const std::string& alpha2_code);

    /**
     * @brief Get flag icon for a business centre by its code.
     *
     * Chains business centre code -> country alpha-2 -> country flag icon.
     * Returns empty icon if any mapping is missing.
     */
    QIcon getBusinessCentreFlagIcon(const std::string& bc_code);

    /**
     * @brief Get flag icon for a calendar by its code.
     *
     * Uses the calendar's own image_id when set (e.g. a central bank's own
     * logo, or a currency-union flag for a supranational calendar);
     * otherwise chains calendar code -> country_code -> country flag icon.
     * Returns empty icon if neither is available.
     */
    QIcon getCalendarFlagIcon(const std::string& calendar_code);

signals:
    /**
     * @brief Emitted when images have been loaded.
     */
    void imagesLoaded();

    /**
     * @brief Emitted when all data has been loaded (after loadAll()).
     */
    void allLoaded();

    /**
     * @brief Emitted when an error occurs during loading.
     */
    void loadError(const QString& error_message);

    /**
     * @brief Emitted when image list has been loaded.
     */
    void imageListLoaded();

    /**
     * @brief Emitted when a single image has been loaded.
     *
     * Connect to this signal to refresh UI when on-demand images finish loading.
     */
    void imageLoaded(const QString& image_id);

    /**
     * @brief Emitted when all available images have been loaded.
     */
    void allAvailableImagesLoaded();

    /**
     * @brief Emitted when currency image assignment is complete.
     */
    void currencyImageSet(const QString& iso_code, bool success, const QString& message);

    /**
     * @brief Emitted when country image assignment is complete.
     */
    void countryImageSet(const QString& alpha2_code, bool success, const QString& message);

private slots:
    void onCurrencyImageIdsLoaded();
    void onCountryImageIdsLoaded();
    void onBusinessCentreMappingLoaded();
    void onCalendarMappingLoaded();
    void onImagesLoaded();
    void onImageListLoaded();
    void onCurrencyImageSet();
    void onCountryImageSet();
    void onAllAvailableImagesLoaded();
    void onIncrementalChangesLoaded();

private:
    /**
     * @brief Convert format-agnostic image data to QIcon.
     *
     * @param data Raw image bytes (as returned by ores.assets.domain::image)
     * @param mime_type MIME type of the data (e.g. "image/svg+xml", "image/jpeg")
     * @return QIcon rendered from the data, or empty icon on failure
     */
    static QIcon dataToIcon(const std::vector<std::uint8_t>& data, const std::string& mime_type);

    /**
     * @brief Load a specific image by ID (internal use).
     *
     * @param image_id The image ID to load
     */
    void loadImageById(const std::string& image_id);

    /**
     * @brief Load currency image IDs for preloading.
     */
    void loadCurrencyImageIds();

    /**
     * @brief Load country image IDs for preloading.
     */
    void loadCountryImageIds();

    /**
     * @brief Load images by their IDs.
     */
    void loadImagesByIds(const std::vector<std::string>& image_ids);

    /**
     * @brief Complete the loadAll() chain by fetching pending_image_ids_,
     * deferring until available_images_ (and its size_bytes) is populated
     * if the image list hasn't loaded yet.
     *
     * Called from every exit point of the currency/country/BC/calendar
     * chain loadAll() kicks off. Without this, pending_image_ids_ would
     * be fetched via loadImagesByIds() before loadImageList() (fired
     * independently, e.g. by MainWindow at login) has necessarily
     * completed -- every id would then be treated as unknown-size and
     * batched one request per image, defeating byte-size-aware batching
     * on this common path purely due to incidental async ordering.
     *
     * Only defers when connected: loadImageList() early-returns without
     * ever setting its QFuture when there's no live connection, so
     * onImageListLoaded() -- the only place a deferred wait gets resumed
     * -- would never fire, permanently stalling pending_ids_await_list_
     * and load_all_in_progress_ and deadlocking every future
     * loadAll()/reload() call. Deferring buys nothing while disconnected
     * anyway (a real fetch can't happen either way), so this fetches
     * immediately in that case instead, same as before this method
     * existed.
     */
    void finishLoadAllChain();

    /**
     * @brief Resume a loadAll() chain deferred by finishLoadAllChain(),
     * if one is pending. Called from onImageListLoaded() regardless of
     * whether the list load succeeded, so a failure can't leave
     * pending_image_ids_ (and load_all_in_progress_) stuck forever --
     * loadImagesByIds() falls back to its existing unknown-size handling.
     */
    void resumeDeferredLoadAllChain();

    /**
     * @brief Load business centre -> country alpha-2 mapping.
     */
    void loadBusinessCentreMapping();

    /**
     * @brief Load calendar -> image_id and calendar -> country alpha-2
     * mappings, and queue any calendar-owned image_ids for preloading.
     */
    void loadCalendarMapping();

    /**
     * @brief Load only images that have changed since last load.
     *
     * Uses the modified_since parameter to fetch only changed images.
     */
    void loadIncrementalChanges();

    struct ImageIdsResult {
        bool success;
        std::vector<std::string> image_ids;
        // Code -> image_id mappings populated during fetch
        std::unordered_map<std::string, std::string> code_to_image_id;
        // Full metadata (incl. size_bytes) for the fetched ids, so callers
        // that know sizes can merge them into available_images_ for
        // byte-size-aware batching later. Only populated by
        // loadIncrementalChanges(); other ImageIdsResult producers
        // (currency/country id loads) leave this empty.
        std::vector<assets::messaging::image_info> image_infos;
    };

    struct BusinessCentreMappingResult {
        bool success;
        std::unordered_map<std::string, std::string> bc_to_country;
    };

    struct CalendarMappingResult {
        bool success;
        std::unordered_map<std::string, std::string> calendar_to_image_id;
        std::unordered_map<std::string, std::string> calendar_to_country;
    };

    struct ImagesResult {
        bool success;
        std::vector<assets::domain::image> images;
        int failed_batches{0}; ///< Number of batches that failed (e.g., due to CRC errors)
    };

    /**
     * @brief Fetch images in batches from the server, sized by cumulative
     * estimated payload rather than image count alone.
     *
     * Each batch is closed -- and a get_images_request issued for it --
     * before adding an image whose size_bytes would push the batch's
     * running estimated-encoded-size total past safe_batch_bytes, so no
     * batch risks exceeding NATS's max payload regardless of how the
     * requested images happen to cluster by size. images_to_fetch entries
     * with size_bytes == 0 (unknown -- e.g. a single-id lookup issued
     * before any image list was ever loaded) are treated as
     * safe_batch_bytes-sized on their own, so an unknown-size image
     * always gets its own batch rather than silently defeating the cap.
     * batch_size (MAX_IMAGES_PER_REQUEST) still applies as a generous
     * sanity bound on top.
     *
     * @param clientManager The client manager to use for requests
     * @param images_to_fetch The images to fetch, with known sizes
     * @return ImagesResult containing fetched images
     */
    static ImagesResult
    fetchImagesInBatches(ClientManager* clientManager,
                         const std::vector<assets::messaging::image_info>& images_to_fetch);

    struct ImageListResult {
        bool success;
        std::vector<assets::messaging::image_info> images;
    };

    struct SingleImageResult {
        bool success;
        std::string image_id;
        assets::domain::image image;
    };

    // Processes one loadImageById() result and emits imageLoaded(). Not a
    // slot: each request gets its own heap-allocated, self-deleting
    // QFutureWatcher (see loadImageById()) rather than sharing one instance
    // -- a single shared watcher would silently drop every request but the
    // most recently assigned future when several are in flight at once
    // (e.g. FlagSelectorDialog's icon grid loading ~20 distinct image_ids).
    void handleSingleImageResult(const SingleImageResult& result);

    struct SetCurrencyImageResult {
        bool success;
        std::string iso_code;
        std::string message;
    };

    struct SetCountryImageResult {
        bool success;
        std::string alpha2_code;
        std::string message;
    };

    ClientManager* clientManager_;

    // image_id -> cached raw image bytes (SVG markup, JPEG, ...)
    std::unordered_map<std::string, std::string> image_data_cache_;

    // image_id -> cached MIME type (e.g. "image/svg+xml", "image/jpeg")
    std::unordered_map<std::string, std::string> image_mime_cache_;

    // image_id -> QIcon (rendered from image data)
    std::unordered_map<std::string, QIcon> image_icons_;

    // Loading state
    bool is_loading_images_{false};
    bool is_loading_all_available_{false};
    bool load_all_in_progress_{false};

    // Set by finishLoadAllChain() when the loadAll() chain reaches its end
    // before available_images_ is populated; resumeDeferredLoadAllChain()
    // clears it and fetches pending_image_ids_ once the list arrives.
    bool pending_ids_await_list_{false};

    // Image IDs collected during loadAll() for preloading
    std::vector<std::string> pending_image_ids_;

    QFutureWatcher<ImageIdsResult>* currency_ids_watcher_;
    QFutureWatcher<ImageIdsResult>* country_ids_watcher_;
    QFutureWatcher<ImageIdsResult>* incremental_changes_watcher_;
    QFutureWatcher<ImagesResult>* images_watcher_;
    QFutureWatcher<ImageListResult>* image_list_watcher_;
    QFutureWatcher<SetCurrencyImageResult>* set_currency_image_watcher_;
    QFutureWatcher<SetCountryImageResult>* set_country_image_watcher_;
    QFutureWatcher<ImagesResult>* all_available_watcher_;

    // List of all available images (metadata only)
    std::vector<assets::messaging::image_info> available_images_;

    // Track image IDs currently being loaded to prevent duplicate requests
    std::unordered_set<std::string> pending_image_requests_;

    // Timestamp of last successful load (for incremental loading)
    std::optional<std::chrono::system_clock::time_point> last_load_time_;

    // Code -> image_id mappings for flag icon lookups
    std::unordered_map<std::string, std::string> currency_iso_to_image_id_;
    std::unordered_map<std::string, std::string> country_alpha2_to_image_id_;
    std::unordered_map<std::string, std::string> bc_code_to_country_alpha2_;
    std::unordered_map<std::string, std::string> calendar_code_to_image_id_;
    std::unordered_map<std::string, std::string> calendar_code_to_country_alpha2_;

    QFutureWatcher<BusinessCentreMappingResult>* bc_mapping_watcher_;
    QFutureWatcher<CalendarMappingResult>* calendar_mapping_watcher_;
};

}

#endif
