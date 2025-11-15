/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_ABOUT_DIALOG_HPP
#define ORES_QT_ABOUT_DIALOG_HPP

#include <QDialog>
#include "ui_AboutDialog.h"
#include "ores.qt/LogoLabel.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace Ui {

class AboutDialog;

}

namespace ores::qt {

/**
 * @brief Modal dialog displaying application version and build metadata.
 *
 * Presents key runtime information such as the application version, build
 * identifier, and associated branding (e.g., logo or splash image). Used
 * primarily for diagnostics and user-facing transparency.
 *
 * The dialog initializes its UI, loads the application logo, and populates
 * version/build labels using compile-time metadata.
 */
class AboutDialog final : public QDialog {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.about_dialog");
        return instance;
    }

public:
    explicit AboutDialog(QWidget* parent = nullptr);
    ~AboutDialog() override;

private:
    void showEvent(QShowEvent* e) override;

    /**
     * @brief Populate all version and build-related UI labels.
     *
     * Retrieves the application version and build metadata (e.g., build
     * timestamp, commit information) and updates the corresponding labels in
     * the dialog. This function performs only UI updates; it does not compute
     * or cache any version data.
     *
     * Call this after the UI is initialised.
     */
    void updateVersionLabels();

private:
    Ui::AboutDialog ui_;
    LogoLabel* logoLabel_;
};

}

#endif
