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
#ifndef ORES_QT_FLAG_SELECTOR_DIALOG_HPP
#define ORES_QT_FLAG_SELECTOR_DIALOG_HPP

#include <QDialog>
#include <QListWidget>
#include <QPushButton>
#include <QLineEdit>
#include <QLabel>
#include <QString>
#include "ores.qt/ImageCache.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Dialog for selecting a flag/image to associate with a currency.
 *
 * Shows a searchable list of available flag images with preview.
 */
class FlagSelectorDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.flag_selector_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a flag selector dialog.
     *
     * @param imageCache The image cache to use for loading flag images
     * @param currentImageId The currently selected image ID (empty if none)
     * @param parent The parent widget
     */
    explicit FlagSelectorDialog(ImageCache* imageCache,
        const QString& currentImageId = QString(),
        QWidget* parent = nullptr);

    /**
     * @brief Get the selected image ID.
     *
     * @return The selected image ID, or empty string if none/cleared
     */
    QString selectedImageId() const { return selectedImageId_; }

private slots:
    void onImageListLoaded();
    void onImageLoaded(const QString& image_id);
    void onAllImagesLoaded();
    void onSearchTextChanged(const QString& text);
    void onItemSelectionChanged();
    void onClearClicked();
    void onSelectClicked();

private:
    void setupUi();
    void populateList();
    void filterList(const QString& filter);
    void loadVisibleIcons();

    ImageCache* imageCache_;
    QString selectedImageId_;
    QString currentImageId_;

    QLineEdit* searchEdit_;
    QListWidget* listWidget_;
    QLabel* previewLabel_;
    QLabel* previewDescLabel_;
    QPushButton* clearButton_;
    QPushButton* selectButton_;
    QPushButton* cancelButton_;
};

}

#endif
