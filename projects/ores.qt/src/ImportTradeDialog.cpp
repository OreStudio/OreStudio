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
#include "ores.qt/ImportTradeDialog.hpp"

#include <set>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QHeaderView>
#include <QComboBox>
#include <QPointer>
#include <QtConcurrent/QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.ore/xml/importer.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ImportTradeDialog::ImportTradeDialog(
    const refdata::domain::book& book,
    const std::vector<ore::xml::trade_import_item>& items,
    const QString& source_label,
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : QDialog(parent),
      book_(book),
      items_(items),
      source_label_(source_label),
      clientManager_(clientManager),
      username_(username),
      mappingSection_(nullptr),
      mappingStatusLabel_(nullptr),
      mappingTable_(nullptr),
      importInProgress_(false),
      cancelRequested_(false) {

    BOOST_LOG_SEV(lg(), debug) << "Creating import trade dialog for book: "
                               << book.name << " with " << items.size()
                               << " trades from: "
                               << source_label.toStdString();

    // Validate all trades
    validation_errors_.reserve(items.size());
    for (const auto& item : items) {
        validation_errors_.push_back(
            ore::xml::importer::validate_trade(item.trade));
    }

    buildUniqueCpNames();
    setupUI();
    populateTradeTable();
    updateSelectionCount();

    // Fetch counterparties asynchronously
    loadCounterparties();
}

ImportTradeDialog::~ImportTradeDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying import trade dialog";
}

void ImportTradeDialog::buildUniqueCpNames() {
    std::set<std::string> seen;
    for (const auto& item : items_) {
        if (!item.ore_counterparty_name.empty()) {
            seen.insert(item.ore_counterparty_name);
        }
    }
    unique_cp_names_.assign(seen.begin(), seen.end());
    BOOST_LOG_SEV(lg(), debug) << "Found " << unique_cp_names_.size()
                               << " unique ORE counterparty names";
}

void ImportTradeDialog::setupUI() {
    setWindowTitle("Import ORE Portfolio Trades");
    setModal(true);
    resize(900, 650);

    auto* mainLayout = new QVBoxLayout(this);

    // Source info (file or directory, depending on how the dialog was opened)
    fileLabel_ = new QLabel(QString("Source: %1").arg(source_label_));
    mainLayout->addWidget(fileLabel_);

    // Book info
    const QString bookStr = QString("Book: %1")
        .arg(QString::fromStdString(book_.name));
    bookLabel_ = new QLabel(bookStr);
    mainLayout->addWidget(bookLabel_);

    // Trades section
    auto* tradesGroup = new QGroupBox(tr("Trades to Import"), this);
    auto* tradesLayout = new QVBoxLayout(tradesGroup);

    // Select all + count row
    auto* selectionLayout = new QHBoxLayout();
    selectAllCheckbox_ = new QCheckBox(tr("Select All"));
    selectAllCheckbox_->setChecked(true);
    QT_WARNING_PUSH
    QT_WARNING_DISABLE_DEPRECATED
    connect(selectAllCheckbox_, &QCheckBox::stateChanged,
            this, &ImportTradeDialog::onSelectAllChanged);
    QT_WARNING_POP
    selectionLayout->addWidget(selectAllCheckbox_);

    selectionCountLabel_ = new QLabel();
    selectionLayout->addWidget(selectionCountLabel_);
    selectionLayout->addStretch();
    tradesLayout->addLayout(selectionLayout);

    // Trade preview table
    tradeTable_ = new QTableWidget(this);
    tradeTable_->setColumnCount(5);
    tradeTable_->setHorizontalHeaderLabels({
        "", "External ID", "Trade Type", "ORE Counterparty", "Netting Set ID"
    });
    tradeTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tradeTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tradeTable_->horizontalHeader()->setStretchLastSection(true);
    tradeTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    tradeTable_->horizontalHeader()->setSectionResizeMode(
        4, QHeaderView::Stretch);
    tradeTable_->verticalHeader()->setVisible(false);
    tradesLayout->addWidget(tradeTable_);

    mainLayout->addWidget(tradesGroup);

    // Counterparty mapping section (only if there are ORE counterparties)
    if (!unique_cp_names_.empty()) {
        mappingSection_ = new QGroupBox(tr("Counterparty Mapping"), this);
        auto* mappingLayout = new QVBoxLayout(mappingSection_);

        mappingStatusLabel_ = new QLabel(tr("Loading ORES counterparties..."));
        mappingLayout->addWidget(mappingStatusLabel_);

        mappingTable_ = new QTableWidget(this);
        mappingTable_->setColumnCount(2);
        mappingTable_->setHorizontalHeaderLabels({
            "ORE CounterParty", "ORES Counterparty"
        });
        mappingTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        mappingTable_->horizontalHeader()->setStretchLastSection(true);
        mappingTable_->horizontalHeader()->setSectionResizeMode(
            0, QHeaderView::ResizeToContents);
        mappingTable_->verticalHeader()->setVisible(false);
        mappingTable_->setVisible(false);
        mappingLayout->addWidget(mappingTable_);

        auto* noteLabel = new QLabel(
            tr("Note: Unmapped counterparties will be imported without a "
               "counterparty reference."));
        noteLabel->setWordWrap(true);
        mappingLayout->addWidget(noteLabel);

        mainLayout->addWidget(mappingSection_);
    }

    // Progress bar
    progressBar_ = new QProgressBar(this);
    progressBar_->setVisible(false);
    progressBar_->setTextVisible(true);
    mainLayout->addWidget(progressBar_);

    // Status label
    statusLabel_ = new QLabel();
    statusLabel_->setVisible(false);
    mainLayout->addWidget(statusLabel_);

    // Buttons
    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch();

    importButton_ = new QPushButton(tr("Import"));
    connect(importButton_, &QPushButton::clicked,
            this, &ImportTradeDialog::onImportClicked);
    buttonLayout->addWidget(importButton_);

    cancelButton_ = new QPushButton(tr("Cancel"));
    connect(cancelButton_, &QPushButton::clicked,
            this, &ImportTradeDialog::onCancelClicked);
    buttonLayout->addWidget(cancelButton_);

    mainLayout->addLayout(buttonLayout);
}

void ImportTradeDialog::populateTradeTable() {
    BOOST_LOG_SEV(lg(), debug) << "Populating trade table with "
                               << items_.size() << " trades";

    tradeTable_->setRowCount(static_cast<int>(items_.size()));

    int valid_count = 0;
    int invalid_count = 0;

    for (size_t i = 0; i < items_.size(); ++i) {
        const auto& item = items_[i];
        const auto& validation_error = validation_errors_[i];
        const bool is_valid = validation_error.empty();

        // Checkbox column
        auto* checkBoxWidget = new QWidget();
        auto* checkBoxLayout = new QHBoxLayout(checkBoxWidget);
        checkBoxLayout->setContentsMargins(0, 0, 0, 0);
        checkBoxLayout->setAlignment(Qt::AlignCenter);

        auto* checkBox = new QCheckBox();
        checkBox->setProperty("row", static_cast<int>(i));
        QT_WARNING_PUSH
        QT_WARNING_DISABLE_DEPRECATED
        connect(checkBox, &QCheckBox::stateChanged,
                this, &ImportTradeDialog::onTradeCheckChanged);
        QT_WARNING_POP

        if (!is_valid) {
            checkBox->setChecked(false);
            checkBox->setEnabled(false);
            checkBoxWidget->setToolTip(
                QString("Cannot import: %1")
                .arg(QString::fromStdString(validation_error)));
            invalid_count++;
        } else {
            checkBox->setChecked(true);
            valid_count++;
        }

        checkBoxLayout->addWidget(checkBox);
        tradeTable_->setCellWidget(static_cast<int>(i), 0, checkBoxWidget);

        // Data columns
        const QString source_tooltip =
            QString("Source: %1")
            .arg(QString::fromStdString(item.source_file.generic_string()));

        auto makeItem = [&](const std::string& text) {
            auto* it = new QTableWidgetItem(QString::fromStdString(text));
            it->setToolTip(source_tooltip);
            if (!is_valid) {
                it->setBackground(QBrush(QColor(255, 200, 200)));
                it->setToolTip(QString("Source: %1\nValidation errors:\n%2")
                    .arg(QString::fromStdString(
                         item.source_file.generic_string()))
                    .arg(QString::fromStdString(validation_error)));
            }
            return it;
        };

        tradeTable_->setItem(static_cast<int>(i), 1,
            makeItem(item.trade.external_id));
        tradeTable_->setItem(static_cast<int>(i), 2,
            makeItem(item.trade.trade_type));
        tradeTable_->setItem(static_cast<int>(i), 3,
            makeItem(item.ore_counterparty_name));
        tradeTable_->setItem(static_cast<int>(i), 4,
            makeItem(item.trade.netting_set_id));
    }

    BOOST_LOG_SEV(lg(), debug) << "Trade table populated: " << valid_count
                               << " valid, " << invalid_count << " invalid";
}

void ImportTradeDialog::populateMappingTable() {
    if (!mappingTable_) return;

    BOOST_LOG_SEV(lg(), debug) << "Populating mapping table with "
                               << unique_cp_names_.size() << " counterparty names";

    mappingTable_->setRowCount(static_cast<int>(unique_cp_names_.size()));
    mapping_combos_.clear();

    for (size_t i = 0; i < unique_cp_names_.size(); ++i) {
        const auto& ore_name = unique_cp_names_[i];

        // ORE name column (read-only)
        auto* nameItem = new QTableWidgetItem(QString::fromStdString(ore_name));
        nameItem->setFlags(nameItem->flags() & ~Qt::ItemIsEditable);
        mappingTable_->setItem(static_cast<int>(i), 0, nameItem);

        // ORES counterparty combo box
        auto* combo = new QComboBox();
        combo->addItem(tr("-- None --"), QString());
        for (const auto& cp : counterparties_) {
            const QString name = QString::fromStdString(cp.full_name);
            const QString id = QString::fromStdString(
                boost::uuids::to_string(cp.id));
            combo->addItem(name, id);
        }

        // Try to auto-match by name (case-insensitive)
        const QString oreLower = QString::fromStdString(ore_name).toLower();
        for (int j = 1; j < combo->count(); ++j) {
            if (combo->itemText(j).toLower() == oreLower) {
                combo->setCurrentIndex(j);
                break;
            }
        }

        mappingTable_->setCellWidget(static_cast<int>(i), 1, combo);
        mapping_combos_[ore_name] = combo;
    }

    mappingTable_->setVisible(true);
    mappingStatusLabel_->setVisible(false);
}

void ImportTradeDialog::loadCounterparties() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        if (mappingStatusLabel_) {
            mappingStatusLabel_->setText(
                tr("Not connected - counterparty mapping unavailable"));
        }
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading counterparties for mapping";

    struct Result {
        bool success = false;
        std::vector<refdata::domain::counterparty> counterparties;
    };

    QPointer<ImportTradeDialog> self = this;

    QFuture<Result> future = QtConcurrent::run([self]() -> Result {
        if (!self || !self->clientManager_)
            return {};

        refdata::messaging::get_counterparties_request request;
        request.offset = 0;
        request.limit = 10000;
        auto payload = request.serialize();

        comms::messaging::frame req_frame(
            comms::messaging::message_type::get_counterparties_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(
            std::move(req_frame));
        if (!response_result) return {};

        auto decompressed = response_result->decompressed_payload();
        if (!decompressed) return {};

        auto response = refdata::messaging::get_counterparties_response::
            deserialize(*decompressed);
        if (!response) return {};

        return {true, std::move(response->counterparties)};
    });

    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, [this, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result.success) {
            if (mappingStatusLabel_) {
                mappingStatusLabel_->setText(
                    tr("Failed to load counterparties"));
            }
            BOOST_LOG_SEV(lg(), warn) << "Failed to load counterparties";
            return;
        }

        counterparties_ = std::move(result.counterparties);
        BOOST_LOG_SEV(lg(), debug) << "Loaded " << counterparties_.size()
                                   << " counterparties";
        populateMappingTable();
    });

    watcher->setFuture(future);
}

void ImportTradeDialog::updateSelectionCount() {
    int selectedCount = 0;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked()) {
                selectedCount++;
            }
        }
    }

    selectionCountLabel_->setText(
        QString("(%1 of %2 trades selected)")
        .arg(selectedCount)
        .arg(items_.size()));

    updateImportButtonState();
}

void ImportTradeDialog::updateImportButtonState() {
    int selectedCount = 0;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked()) {
                selectedCount++;
            }
        }
    }
    importButton_->setEnabled(selectedCount > 0 && !importInProgress_);
}

std::optional<boost::uuids::uuid>
ImportTradeDialog::resolveCounterpartyId(const std::string& ore_name) const {
    if (ore_name.empty()) return std::nullopt;

    auto it = mapping_combos_.find(ore_name);
    if (it == mapping_combos_.end()) return std::nullopt;

    const QComboBox* combo = it->second;
    if (!combo || combo->currentIndex() <= 0) return std::nullopt;

    const QString uuid_str = combo->currentData().toString();
    if (uuid_str.isEmpty()) return std::nullopt;

    try {
        return boost::lexical_cast<boost::uuids::uuid>(uuid_str.toStdString());
    } catch (...) {
        return std::nullopt;
    }
}

void ImportTradeDialog::onSelectAllChanged(int state) {
    BOOST_LOG_SEV(lg(), debug) << "Select all changed: " << state;

    const bool checked = (state == Qt::Checked);
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isEnabled()) {
                checkBox->setChecked(checked);
            }
        }
    }
    updateSelectionCount();
}

void ImportTradeDialog::onTradeCheckChanged() {
    updateSelectionCount();

    // Sync the Select All checkbox
    int checkedCount = 0;
    int enabledCount = 0;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isEnabled()) {
                enabledCount++;
                if (checkBox->isChecked()) {
                    checkedCount++;
                }
            }
        }
    }

    selectAllCheckbox_->blockSignals(true);
    if (checkedCount == 0) {
        selectAllCheckbox_->setCheckState(Qt::Unchecked);
    } else if (checkedCount == enabledCount) {
        selectAllCheckbox_->setCheckState(Qt::Checked);
    } else {
        selectAllCheckbox_->setCheckState(Qt::PartiallyChecked);
    }
    selectAllCheckbox_->blockSignals(false);
}

void ImportTradeDialog::onImportClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Import button clicked";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Import cancelled: client disconnected";
        MessageBoxHelper::warning(this, tr("Disconnected"),
            tr("Cannot import trades while disconnected."));
        return;
    }

    importInProgress_ = true;
    importButton_->setEnabled(false);
    selectAllCheckbox_->setEnabled(false);
    tradeTable_->setEnabled(false);
    if (mappingSection_) mappingSection_->setEnabled(false);

    progressBar_->setVisible(true);
    statusLabel_->setVisible(true);

    // Collect selected trades with resolved mappings
    struct TradeToImport {
        trading::domain::trade trade;
    };

    std::vector<TradeToImport> selected;
    for (int i = 0; i < tradeTable_->rowCount(); ++i) {
        auto* cellWidget = tradeTable_->cellWidget(i, 0);
        if (!cellWidget) continue;
        auto* checkBox = cellWidget->findChild<QCheckBox*>();
        if (!checkBox || !checkBox->isChecked()) continue;

        const auto& item = items_[static_cast<size_t>(i)];

        TradeToImport tti;
        tti.trade = item.trade;

        // Assign UUID for the new trade
        tti.trade.id = boost::uuids::random_generator()();

        // Set book, portfolio, and party from the selected book
        tti.trade.book_id = book_.id;
        tti.trade.portfolio_id = book_.parent_portfolio_id;
        tti.trade.party_id = book_.party_id;

        // Resolve counterparty
        tti.trade.counterparty_id =
            resolveCounterpartyId(item.ore_counterparty_name);

        // Set the username as the modifier
        tti.trade.modified_by = username_.toStdString();

        selected.push_back(std::move(tti));
    }

    const int total = static_cast<int>(selected.size());
    BOOST_LOG_SEV(lg(), info) << "Starting import of " << total << " trades";

    progressBar_->setRange(0, total);
    progressBar_->setValue(0);
    statusLabel_->setText(tr("Starting import..."));

    auto self = this;

    QFuture<std::pair<int, int>> future =
        QtConcurrent::run([self, selected, total]() -> std::pair<int, int> {
            using namespace ores::trading::messaging;

            int success_count = 0;
            int current = 0;

            for (const auto& tti : selected) {
                if (self->cancelRequested_.load()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Import cancelled by user at trade "
                        << current << " of " << total;
                    break;
                }

                current++;

                QMetaObject::invokeMethod(self,
                    [self, current, total, ext_id = tti.trade.external_id]() {
                        self->progressBar_->setValue(current);
                        self->statusLabel_->setText(
                            QString("Importing %1 (%2 of %3)...")
                            .arg(QString::fromStdString(ext_id))
                            .arg(current)
                            .arg(total));
                    }, Qt::QueuedConnection);

                try {
                    save_trade_request request{tti.trade};
                    auto payload = request.serialize();
                    comms::messaging::frame request_frame(
                        comms::messaging::message_type::save_trade_request,
                        0, std::move(payload));

                    auto response_result = self->clientManager_->sendRequest(
                        std::move(request_frame));

                    if (!response_result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to import trade: "
                            << tti.trade.external_id;
                        continue;
                    }

                    auto payload_result =
                        response_result->decompressed_payload();
                    if (!payload_result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to decompress response for: "
                            << tti.trade.external_id;
                        continue;
                    }

                    auto response = save_trade_response::
                        deserialize(*payload_result);

                    if (response && response->success) {
                        success_count++;
                        BOOST_LOG_SEV(lg(), debug)
                            << "Successfully imported trade: "
                            << tti.trade.external_id;
                    } else {
                        const std::string msg = response
                            ? response->message : "Unknown error";
                        BOOST_LOG_SEV(lg(), warn)
                            << "Server rejected trade: "
                            << tti.trade.external_id << " - " << msg;
                    }
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Error importing trade "
                        << tti.trade.external_id << ": " << e.what();
                }
            }

            return {success_count, total};
        });

    auto* watcher = new QFutureWatcher<std::pair<int, int>>(this);
    connect(watcher, &QFutureWatcher<std::pair<int, int>>::finished,
            this, [this, watcher]() {
        auto result = watcher->result();
        const int success_count = result.first;
        const int total_count = result.second;

        progressBar_->setVisible(false);
        statusLabel_->setVisible(false);
        importInProgress_ = false;

        if (cancelRequested_.load()) {
            BOOST_LOG_SEV(lg(), info)
                << "Import cancelled after importing " << success_count
                << " of " << total_count << " trades";
            emit importCancelled();
            reject();
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Import completed: " << success_count
                << " of " << total_count << " trades imported successfully";
            emit importCompleted(success_count, total_count);
            accept();
        }

        watcher->deleteLater();
    });

    watcher->setFuture(future);
}

void ImportTradeDialog::onCancelClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Cancel button clicked";

    if (importInProgress_) {
        cancelRequested_.store(true);
        BOOST_LOG_SEV(lg(), info) << "Cancellation requested";
        statusLabel_->setText(tr("Cancelling import..."));
        cancelButton_->setEnabled(false);
    } else {
        reject();
    }
}

}
