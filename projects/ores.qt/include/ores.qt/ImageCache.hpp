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

#include <unordered_map>
#include <unordered_set>
#include <QObject>
#include <QIcon>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.assets/domain/image.hpp"
#include "ores.assets/messaging/assets_protocol.hpp"

namespace ores::qt {

/**
 * @brief Cache for dynamically loaded images (flags, icons) from the server.
 *
 * This class manages the fetching and caching of images from the server.
 * It provides a simple interface to get icons for currencies by their ISO code.
 *
 * Typical usage:
 * 1. Call loadCurrencyMappings() to fetch currency->image mappings
 * 2. Call loadImagesForCurrencies() to fetch actual image data
 * 3. Use getCurrencyIcon() to retrieve cached icons
 */
class ImageCache final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.image_cache";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ImageCache(ClientManager* clientManager, QObject* parent = nullptr);
    ~ImageCache() override = default;

    /**
     * @brief Load currency-image mappings from the server.
     *
     * Fetches all currency->image_id mappings. After completion,
     * currencyMappingsLoaded() signal is emitted.
     */
    void loadCurrencyMappings();

    /**
     * @brief Load images for all currencies that have mappings.
     *
     * Fetches image data for currencies that have mappings but whose
     * images haven't been loaded yet. Images are fetched in batches.
     * After completion, imagesLoaded() signal is emitted.
     */
    void loadImagesForCurrencies();

    /**
     * @brief Load all currency flags in one operation.
     *
     * Convenience method that loads mappings first, then loads images.
     */
    void loadAll();

    /**
     * @brief Get the icon for a currency.
     *
     * @param iso_code The currency's ISO code (e.g., "USD", "EUR")
     * @return QIcon for the currency's flag, or empty icon if not cached
     */
    QIcon getCurrencyIcon(const std::string& iso_code) const;

    /**
     * @brief Check if the cache has an icon for the given currency.
     *
     * @param iso_code The currency's ISO code
     * @return true if icon is cached, false otherwise
     */
    bool hasCurrencyIcon(const std::string& iso_code) const;

    /**
     * @brief Load country-image mappings from the server.
     *
     * Fetches all country->image_id mappings. After completion,
     * countryMappingsLoaded() signal is emitted.
     */
    void loadCountryMappings();

    /**
     * @brief Load images for all countries that have mappings.
     *
     * Fetches image data for countries that have mappings but whose
     * images haven't been loaded yet. Images are fetched in batches.
     */
    void loadImagesForCountries();

    /**
     * @brief Get the icon for a country.
     *
     * @param alpha2_code The country's ISO 3166-1 alpha-2 code (e.g., "US", "GB")
     * @return QIcon for the country's flag, or empty icon if not cached
     */
    QIcon getCountryIcon(const std::string& alpha2_code) const;

    /**
     * @brief Check if the cache has an icon for the given country.
     *
     * @param alpha2_code The country's alpha-2 code
     * @return true if icon is cached, false otherwise
     */
    bool hasCountryIcon(const std::string& alpha2_code) const;

    /**
     * @brief Get the number of cached currency icons.
     */
    std::size_t cachedIconCount() const { return currency_icons_.size(); }

    /**
     * @brief Check if images are currently being loaded.
     */
    bool isLoading() const { return is_loading_mappings_ || is_loading_images_; }

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
     * @brief Load a specific image by ID for preview.
     *
     * @param image_id The image ID to load
     */
    void loadImageById(const std::string& image_id);

    /**
     * @brief Load all available images from the image list.
     *
     * Fetches SVG data for all images in the available_images_ list.
     * After completion, allAvailableImagesLoaded() signal is emitted.
     */
    void loadAllAvailableImages();

    /**
     * @brief Get icon for an image ID (from preview cache).
     *
     * @param image_id The image ID
     * @return QIcon for the image, or empty icon if not cached
     */
    QIcon getImageIcon(const std::string& image_id) const;

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
     * @brief Get the image ID assigned to a currency.
     *
     * @param iso_code The currency ISO code
     * @return The image ID, or empty string if no mapping
     */
    std::string getCurrencyImageId(const std::string& iso_code) const;

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

signals:
    /**
     * @brief Emitted when currency mappings have been loaded.
     */
    void currencyMappingsLoaded();

    /**
     * @brief Emitted when country mappings have been loaded.
     */
    void countryMappingsLoaded();

    /**
     * @brief Emitted when images have been loaded.
     */
    void imagesLoaded();

    /**
     * @brief Emitted when all data (mappings + images) has been loaded.
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
     * @brief Emitted when a single image has been loaded for preview.
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

private slots:
    void onMappingsLoaded();
    void onCountryMappingsLoaded();
    void onImagesLoaded();
    void onImageListLoaded();
    void onSingleImageLoaded();
    void onCurrencyImageSet();
    void onAllAvailableImagesLoaded();
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds);

private:
    /**
     * @brief Convert SVG string data to QIcon.
     *
     * @param svg_data The SVG content as a string
     * @return QIcon rendered from the SVG, or empty icon on failure
     */
    static QIcon svgToIcon(const std::string& svg_data);

    struct MappingsResult {
        bool success;
        std::unordered_map<std::string, std::string> mappings;  // iso_code -> image_id
    };

    struct ImagesResult {
        bool success;
        std::vector<assets::domain::image> images;
    };

    /**
     * @brief Fetch images in batches from the server.
     *
     * This is a helper method used by both loadImagesForCurrencies and
     * loadAllAvailableImages to avoid code duplication.
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

    ClientManager* clientManager_;

    // Currency ISO code -> image_id mapping
    std::unordered_map<std::string, std::string> currency_to_image_id_;

    // Country alpha-2 code -> image_id mapping
    std::unordered_map<std::string, std::string> country_to_image_id_;

    // image_id -> cached SVG data (for images we've fetched)
    std::unordered_map<std::string, std::string> image_svg_cache_;

    // Currency ISO code -> QIcon (final rendered icons)
    std::unordered_map<std::string, QIcon> currency_icons_;

    // Country alpha-2 code -> QIcon (final rendered icons)
    std::unordered_map<std::string, QIcon> country_icons_;

    // Loading state
    bool is_loading_mappings_{false};
    bool is_loading_country_mappings_{false};
    bool is_loading_images_{false};
    bool is_loading_all_available_{false};
    bool load_images_after_mappings_{false};

    QFutureWatcher<MappingsResult>* mappings_watcher_;
    QFutureWatcher<MappingsResult>* country_mappings_watcher_;
    QFutureWatcher<ImagesResult>* images_watcher_;
    QFutureWatcher<ImageListResult>* image_list_watcher_;
    QFutureWatcher<SingleImageResult>* single_image_watcher_;
    QFutureWatcher<SetCurrencyImageResult>* set_currency_image_watcher_;
    QFutureWatcher<ImagesResult>* all_available_watcher_;

    // List of all available images (metadata only)
    std::vector<assets::messaging::image_info> available_images_;

    // image_id -> QIcon cache for preview (loaded on demand)
    std::unordered_map<std::string, QIcon> image_preview_cache_;

    // Track image IDs currently being loaded to prevent duplicate requests
    std::unordered_set<std::string> pending_image_requests_;

    // ISO codes that need selective refresh (from notifications)
    std::vector<std::string> pending_refresh_iso_codes_;
};

}

#endif
