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
#ifndef ORES_QT_IMPORT_CURRENCY_DIALOG_HPP
#define ORES_QT_IMPORT_CURRENCY_DIALOG_HPP

#include <QDialog>
#include <QTableWidget>
#include <QProgressBar>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QString>
#include <QFutureWatcher>
#include <vector>
#include <memory>
#include "ores.risk/domain/currency.hpp"
#include "ores.comms/net/client.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Dialog for previewing and importing currencies from XML files.
 *
 * Provides a table view of all currencies to be imported with:
 * - Checkboxes for selecting which currencies to import
 * - Full preview of currency details
 * - Progress bar showing import progress
 * - Status updates during import
 * - Cancel capability during import
 */
class ImportCurrencyDialog final : public QDialog {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.import_currency_dialog");
        return instance;
    }

public:
    /**
     * @brief Constructs the import dialog.
     * @param currencies Currencies parsed from XML file
     * @param filename Path to the XML file being imported
     * @param client Client connection for sending currencies to server
     * @param parent Parent widget
     */
    explicit ImportCurrencyDialog(
        const std::vector<risk::domain::currency>& currencies,
        const QString& filename,
        std::shared_ptr<comms::net::client> client,
        QWidget* parent = nullptr);

    ~ImportCurrencyDialog() override;

    /**
     * @brief Get the currencies selected for import.
     * @return Vector of selected currencies
     */
    [[nodiscard]] std::vector<risk::domain::currency>
    getSelectedCurrencies() const;

signals:
    /**
     * @brief Emitted when import completes successfully.
     * @param success_count Number of currencies successfully imported
     * @param total_count Total number of currencies attempted
     */
    void importCompleted(int success_count, int total_count);

    /**
     * @brief Emitted when import is cancelled.
     */
    void importCancelled();

private slots:
    /**
     * @brief Handles the Import button click.
     */
    void onImportClicked();

    /**
     * @brief Handles the Cancel button click.
     */
    void onCancelClicked();

    /**
     * @brief Handles Select All checkbox state change.
     */
    void onSelectAllChanged(int state);

    /**
     * @brief Handles individual currency checkbox state change.
     */
    void onCurrencyCheckChanged();

private:
    /**
     * @brief Sets up the UI components.
     */
    void setupUI();

    /**
     * @brief Populates the table with currency data.
     */
    void populateTable();

    /**
     * @brief Updates the Import button enabled state.
     */
    void updateImportButtonState();

    /**
     * @brief Updates the selection count label.
     */
    void updateSelectionCount();

private:
    std::vector<risk::domain::currency> currencies_;
    QString filename_;
    std::shared_ptr<comms::net::client> client_;

    // UI Components
    QLabel* filenameLabel_;
    QCheckBox* selectAllCheckbox_;
    QLabel* selectionCountLabel_;
    QTableWidget* currencyTable_;
    QProgressBar* progressBar_;
    QLabel* statusLabel_;
    QPushButton* importButton_;
    QPushButton* cancelButton_;

    bool importInProgress_;
};

}

#endif
