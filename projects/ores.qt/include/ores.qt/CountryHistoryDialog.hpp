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
#ifndef ORES_QT_COUNTRY_HISTORY_DIALOG_HPP
#define ORES_QT_COUNTRY_HISTORY_DIALOG_HPP

#include <memory>
#include <QPair>
#include <QWidget>
#include <QString>
#include <QVector>
#include <QToolBar>
#include <QAction>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.refdata/domain/country.hpp"
#include "ores.logging/make_logger.hpp"
#include "ui_CountryHistoryDialog.h"

namespace Ui {
class CountryHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Widget for displaying country version history.
 */
class CountryHistoryDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.country_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    const QIcon& getHistoryIcon() const;

public:
    explicit CountryHistoryDialog(QString alpha2_code,
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~CountryHistoryDialog() override;

    void loadHistory();

    /**
     * @brief Set the image cache for displaying country flags.
     */
    void setImageCache(ImageCache* imageCache);

    QSize sizeHint() const override;

    /**
     * @brief Mark the history data as stale and reload.
     *
     * Called when a notification is received indicating this country has
     * changed on the server. Automatically reloads the history data.
     */
    void markAsStale();

    /**
     * @brief Returns the alpha-2 code of the country.
     */
    [[nodiscard]] QString alpha2Code() const { return alpha2Code_; }

    /**
     * @brief Returns the loaded history vector for version navigation.
     */
    [[nodiscard]] const std::vector<refdata::domain::country>& getHistory() const {
        return history_;
    }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

    /**
     * @brief Emitted when user requests to open a version in read-only mode.
     * @param country The country data at the selected version.
     * @param versionNumber The version number being viewed.
     */
    void openVersionRequested(const refdata::domain::country& country, int versionNumber);

    /**
     * @brief Emitted when user requests to revert to a selected version.
     * @param country The country data to revert to.
     */
    void revertVersionRequested(const refdata::domain::country& country);

private slots:
    void onVersionSelected(int index);
    void onHistoryLoaded();
    void onHistoryLoadError(const QString& error);
    void onOpenClicked();
    void onRevertClicked();
    void onReloadClicked();

private:
    void displayChangesTab(int version_index);
    void displayFullDetailsTab(int version_index);

    /**
     * @brief Calculate differences between two versions.
     *
     * @return Vector of (field_name, (old_value, new_value)) pairs.
     */
    using DiffResult = QVector<QPair<QString, QPair<QString, QString>>>;
    DiffResult calculateDiff(
        const refdata::domain::country& current,
        const refdata::domain::country& previous);

    void setupToolbar();
    void updateButtonStates();
    int selectedVersionIndex() const;

    std::unique_ptr<Ui::CountryHistoryDialog> ui_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString alpha2Code_;
    std::vector<refdata::domain::country> history_;

    QToolBar* toolBar_;
    QAction* reloadAction_;
    QAction* openAction_;
    QAction* revertAction_;
};

}

#endif
