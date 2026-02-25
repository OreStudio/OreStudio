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
#ifndef ORES_QT_IMPORT_TRADE_DIALOG_HPP
#define ORES_QT_IMPORT_TRADE_DIALOG_HPP

#include <atomic>
#include <map>
#include <vector>
#include <optional>
#include <QString>
#include <QDialog>
#include <QLabel>
#include <QWidget>
#include <QCheckBox>
#include <QPushButton>
#include <QComboBox>
#include <QTableWidget>
#include <QProgressBar>
#include <QFutureWatcher>
#include <boost/uuid/uuid.hpp>
#include "ores.ore/xml/importer.hpp"
#include "ores.refdata/domain/book.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Dialog for mapping and importing ORE portfolio trades into a book.
 *
 * Shows a two-section UI:
 * - Trade preview table with checkboxes for selection
 * - Counterparty mapping section (ORE string names → ORES counterparty UUIDs)
 *
 * ORES counterparties are fetched from the server asynchronously when the
 * dialog opens. Trades can be imported with or without counterparty mappings;
 * unmapped counterparties simply leave counterparty_id as nullopt.
 */
class ImportTradeDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.import_trade_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the import dialog.
     *
     * @param book The ORES book trades will be imported into
     * @param items Trades to import (may come from one file or many in a
     *              batch/directory scenario — each item carries its own
     *              source_file for per-trade attribution)
     * @param source_label Human-readable label shown at the top of the dialog
     *                     (e.g. file name for single-file import, directory
     *                     name for batch import)
     * @param clientManager Client manager for server communication
     * @param username Current logged-in username
     * @param parent Parent widget
     */
    explicit ImportTradeDialog(
        const refdata::domain::book& book,
        const std::vector<ore::xml::trade_import_item>& items,
        const QString& source_label,
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);

    ~ImportTradeDialog() override;

signals:
    /**
     * @brief Emitted when import completes.
     * @param success_count Number of trades successfully imported
     * @param total_count Total number of trades attempted
     */
    void importCompleted(int success_count, int total_count);

    /**
     * @brief Emitted when import is cancelled.
     */
    void importCancelled();

private slots:
    void onImportClicked();
    void onCancelClicked();
    void onSelectAllChanged(int state);
    void onTradeCheckChanged();

private:
    void setupUI();
    void populateTradeTable();
    void buildUniqueCpNames();
    void populateMappingTable();
    void loadCounterparties();
    void updateImportButtonState();
    void updateSelectionCount();

    /**
     * @brief Resolves an ORE counterparty name to an ORES UUID from the mapping table.
     * @param ore_name ORE CounterParty string
     * @return ORES counterparty UUID if mapped, nullopt if unmapped or name is empty
     */
    [[nodiscard]] std::optional<boost::uuids::uuid>
    resolveCounterpartyId(const std::string& ore_name) const;

private:
    refdata::domain::book book_;
    std::vector<ore::xml::trade_import_item> items_;
    std::vector<std::string> validation_errors_;
    std::vector<refdata::domain::counterparty> counterparties_;
    QString source_label_;
    ClientManager* clientManager_;
    QString username_;

    // Unique ORE counterparty names found across all trades
    std::vector<std::string> unique_cp_names_;

    // Maps ORE counterparty name → combo box widget in the mapping table
    std::map<std::string, QComboBox*> mapping_combos_;

    // UI components
    QLabel* fileLabel_;
    QLabel* bookLabel_;
    QCheckBox* selectAllCheckbox_;
    QLabel* selectionCountLabel_;
    QTableWidget* tradeTable_;
    QWidget* mappingSection_;
    QLabel* mappingStatusLabel_;
    QTableWidget* mappingTable_;
    QProgressBar* progressBar_;
    QLabel* statusLabel_;
    QPushButton* importButton_;
    QPushButton* cancelButton_;

    bool importInProgress_;
    std::atomic<bool> cancelRequested_;
};

}

#endif
