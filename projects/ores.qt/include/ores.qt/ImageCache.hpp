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

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <QObject>
#include <QIcon>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.assets/domain/image.hpp"
#include "ores.assets/messaging/assets_protocol.hpp"

namespace ores::qt {

/**
 * @brief Cache for dynamically loaded images (flags, icons) from the server.
 *
 * This class manages the fetching and caching of images from the server.
 * Images are cached by their UUID (image_id) and can be retrieved for any
 * entity that references them.
 *
 * Simplified design:
 * - Single mapping: image_id -> SVG data and rendered QIcon
 * - Entities (currencies, countries) have their own image_id field
 * - Call getIcon(image_id) to get the icon for any image
 * - On-demand loading: if image not cached, loads from server
 *
 * Typical usage:
 * 1. Call loadAll() to preload images for current entities
 * 2. Use getIcon(image_id) to retrieve icons - loads on-demand if missing
 */
class ImageCache final : public QObject {
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
    std::size_t cachedIconCount() const { return image_icons_.size(); }

    /**
     * @brief Check if images are currently being loaded.
     */
    bool isLoading() const { return is_loading_images_; }

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
    bool hasImageList() const { return !available_images_.empty(); }

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
    void setCurrencyImage(const std::string& iso_code, const std::string& image_id,
        const std::string& assigned_by);

    /**
     * @brief Set or remove a country's image association.
     *
     * @param alpha2_code The country alpha-2 code
     * @param image_id The image ID to assign (empty to remove)
     * @param assigned_by Username performing the assignment
     */
    void setCountryImage(const std::string& alpha2_code, const std::string& image_id,
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
    void onImagesLoaded();
    void onImageListLoaded();
    void onSingleImageLoaded();
    void onCurrencyImageSet();
    void onCountryImageSet();
    void onAllAvailableImagesLoaded();
    void onIncrementalChangesLoaded();

private:
    /**
     * @brief Convert SVG string data to QIcon.
     *
     * @param svg_data The SVG content as a string
     * @return QIcon rendered from the SVG, or empty icon on failure
     */
    static QIcon svgToIcon(const std::string& svg_data);

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
     * @brief Load business centre -> country alpha-2 mapping.
     */
    void loadBusinessCentreMapping();

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
    };

    struct BusinessCentreMappingResult {
        bool success;
        std::unordered_map<std::string, std::string> bc_to_country;
    };

    struct ImagesResult {
        bool success;
        std::vector<assets::domain::image> images;
        int failed_batches{0};  ///< Number of batches that failed (e.g., due to CRC errors)
    };

    /**
     * @brief Fetch images in batches from the server.
     *
     * @param clientManager The client manager to use for requests
     * @param image_ids The list of image IDs to fetch
     * @return ImagesResult containing fetched images
     */
    static ImagesResult fetchImagesInBatches(
        ClientManager* clientManager,
        const std::vector<std::string>& image_ids);

    struct ImageListResult {
        bool success;
        std::vector<assets::messaging::image_info> images;
    };

    struct SingleImageResult {
        bool success;
        std::string image_id;
        assets::domain::image image;
    };

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

    // image_id -> cached SVG data
    std::unordered_map<std::string, std::string> image_svg_cache_;

    // image_id -> QIcon (rendered from SVG)
    std::unordered_map<std::string, QIcon> image_icons_;

    // Loading state
    bool is_loading_images_{false};
    bool is_loading_all_available_{false};
    bool load_all_in_progress_{false};

    // Image IDs collected during loadAll() for preloading
    std::vector<std::string> pending_image_ids_;

    QFutureWatcher<ImageIdsResult>* currency_ids_watcher_;
    QFutureWatcher<ImageIdsResult>* country_ids_watcher_;
    QFutureWatcher<ImageIdsResult>* incremental_changes_watcher_;
    QFutureWatcher<ImagesResult>* images_watcher_;
    QFutureWatcher<ImageListResult>* image_list_watcher_;
    QFutureWatcher<SingleImageResult>* single_image_watcher_;
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

    QFutureWatcher<BusinessCentreMappingResult>* bc_mapping_watcher_;
};

}

#endif
