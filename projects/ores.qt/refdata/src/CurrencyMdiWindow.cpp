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
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/ImportEntityDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include <QDesktopServices>
#include <QFile>
#include <QFileDialog>
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QUrl>
#include <QVBoxLayout>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CurrencyMdiWindow::CurrencyMdiWindow(ClientManager* clientManager,
                                     const QString& username,
                                     BadgeCache* badgeCache,
                                     ImageCache* imageCache,
                                     QWidget* parent)
    : EntityListMdiWindow(parent)
    , clientManager_(clientManager)
    , username_(username)
    , badgeCache_(badgeCache)
    , imageCache_(imageCache)
    , toolbar_(nullptr)
    , tableView_(nullptr)
    , model_(nullptr)
    , proxyModel_(nullptr)
    , paginationWidget_(nullptr)
    , reloadAction_(nullptr)
    , addAction_(nullptr)
    , editAction_(nullptr)
    , deleteAction_(nullptr)
    , importXMLAction_(nullptr)
    , exportCSVAction_(nullptr)
    , exportXMLAction_(nullptr)
    , historyAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void CurrencyMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void CurrencyMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this, &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("Add"));
    addAction_->setToolTip(tr("Add new currency"));
    connect(addAction_, &QAction::triggered, this, &CurrencyMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit selected currency"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this, &CurrencyMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected currency"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this, &CurrencyMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor), tr("History"));
    historyAction_->setToolTip(tr("View currency history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this, &CurrencyMdiWindow::viewHistorySelected);

    toolbar_->addSeparator();

    importXMLAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ImportOre, IconUtils::DefaultIconColor),
        tr("Import XML"));
    importXMLAction_->setToolTip(tr("Import currencies from an ORE XML file"));
    connect(importXMLAction_, &QAction::triggered, this, &CurrencyMdiWindow::importFromXML);

    exportCSVAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ExportCsv, IconUtils::DefaultIconColor),
        tr("Export CSV"));
    exportCSVAction_->setToolTip(tr("Export currencies to CSV"));
    connect(exportCSVAction_, &QAction::triggered, this, &CurrencyMdiWindow::exportToCSV);

    exportXMLAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ExportOre, IconUtils::DefaultIconColor),
        tr("Export XML"));
    exportXMLAction_->setToolTip(tr("Export currencies to ORE XML"));
    connect(exportXMLAction_, &QAction::triggered, this, &CurrencyMdiWindow::exportToXML);

    toolbar_->addSeparator();

    {
        auto* action = toolbar_->addAction(
            IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor), tr("Rounding"));
        action->setToolTip(tr("Open Rounding Types list"));
        connect(action, &QAction::triggered, this, [this]() { emit showRoundingTypesRequested(); });
    }

    {
        auto* action = toolbar_->addAction(
            IconUtils::createRecoloredIcon(Icon::Classification, IconUtils::DefaultIconColor),
            tr("Monetary Natures"));
        action->setToolTip(tr("Open Monetary Natures list"));
        connect(
            action, &QAction::triggered, this, [this]() { emit showMonetaryNaturesRequested(); });
    }

    {
        auto* action = toolbar_->addAction(
            IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor),
            tr("Market Tiers"));
        action->setToolTip(tr("Open Currency Market Tiers list"));
        connect(action, &QAction::triggered, this, [this]() { emit showMarketTiersRequested(); });
    }
}

void CurrencyMdiWindow::setupTable() {
    model_ = new ClientCurrencyModel(clientManager_, this);
    model_->setImageCache(imageCache_);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->setIconSize(single_flag_icon_size());

    using cs = column_style;
    auto* delegate = new EntityItemDelegate(
        {
            cs::icon_text_left,
            cs::text_left,
            cs::text_left,
            cs::text_left,
            cs::text_left,
            cs::mono_center,
            cs::text_left,
            cs::mono_center,
            cs::text_left,
            cs::badge_centered,
            cs::badge_centered,
            cs::mono_center,
            cs::text_left,
            cs::mono_center,
            cs::mono_center,
            cs::text_left,
            cs::text_left,
        },
        tableView_);
    delegate->set_badge_color_resolver(
        9, [cache = badgeCache_](const QString& value) -> badge_color_pair {
            static const badge_color_pair fallback{color_constants::badge_fallback,
                                                   color_constants::badge_fallback_text};
            if (!cache)
                return fallback;
            auto* def = cache->resolve("monetary_nature", value.toStdString());
            if (!def)
                return fallback;
            return {QColor(QString::fromStdString(def->background_colour)),
                    QColor(QString::fromStdString(def->text_colour))};
        });
    delegate->set_badge_color_resolver(
        10, [cache = badgeCache_](const QString& value) -> badge_color_pair {
            static const badge_color_pair fallback{color_constants::badge_fallback,
                                                   color_constants::badge_fallback_text};
            if (!cache)
                return fallback;
            auto* def = cache->resolve("currency_market_tier", value.toStdString());
            if (!def)
                return fallback;
            return {QColor(QString::fromStdString(def->background_colour)),
                    QColor(QString::fromStdString(def->text_colour))};
        });
    tableView_->setItemDelegate(delegate);
    if (badgeCache_) {
        if (badgeCache_->isLoaded())
            tableView_->viewport()->update();
        connect(badgeCache_, &BadgeCache::loaded, tableView_->viewport(), [this]() {
            tableView_->viewport()->update();
        });
    }

    initializeTableSettings(tableView_,
                            model_,
                            "CurrencyListWindow",
                            {
                                ClientCurrencyModel::NumericCode,
                                ClientCurrencyModel::FractionSymbol,
                                ClientCurrencyModel::FractionsPerUnit,
                                ClientCurrencyModel::RoundingType,
                                ClientCurrencyModel::Format,
                                ClientCurrencyModel::SpotDays,
                                ClientCurrencyModel::BasePrecedence,
                            },
                            {900, 400},
                            3);
}

void CurrencyMdiWindow::setupConnections() {
    connect(model_, &ClientCurrencyModel::dataLoaded, this, &CurrencyMdiWindow::onDataLoaded);
    connect(model_, &ClientCurrencyModel::loadError, this, &CurrencyMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &CurrencyMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked, this, &CurrencyMdiWindow::onDoubleClicked);

    connect(
        paginationWidget_, &PaginationWidget::page_size_changed, this, [this](std::uint32_t size) {
            model_->set_page_size(size);
            model_->refresh();
        });

    connect(paginationWidget_, &PaginationWidget::load_all_requested, this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            paginationWidget_->reset_page();
            model_->refresh();
        }
    });

    connect(
        paginationWidget_,
        &PaginationWidget::page_requested,
        this,
        [this](std::uint32_t offset, std::uint32_t limit) { model_->load_page(offset, limit); });

    connectModel(model_);
}

void CurrencyMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading currencies";
    clearStaleIndicator();
    emit statusChanged(tr("Loading currencies..."));
    model_->load_page(paginationWidget_->current_offset(), paginationWidget_->page_size());
}

void CurrencyMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 currencies").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void CurrencyMdiWindow::onLoadError(const QString& error_message, const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CurrencyMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void CurrencyMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* currency = model_->getCurrency(sourceIndex.row())) {
        emit showCurrencyDetails(*currency);
    }
}

void CurrencyMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CurrencyMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new currency requested";
    emit addNewRequested();
}

void CurrencyMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* currency = model_->getCurrency(sourceIndex.row())) {
        emit showCurrencyDetails(*currency);
    }
}

void CurrencyMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* currency = model_->getCurrency(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug)
            << "Emitting showCurrencyHistory for code: " << currency->iso_code;
        emit showCurrencyHistory(*currency);
    }
}

void CurrencyMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete currency while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* currency = model_->getCurrency(sourceIndex.row())) {
            codes.push_back(currency->iso_code);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid currencies to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size() << " currencies";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete currency '%1'?")
                             .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage =
            QString("Are you sure you want to delete %1 currencies?").arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(
        this, "Delete Currency", confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<CurrencyMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self)
            return {};

        BOOST_LOG_SEV(lg(), debug) << "Making delete request for " << codes.size() << " currencies";

        refdata::messaging::delete_currency_request request;
        request.iso_codes = codes;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to communicate with server"}});
            }
            return results;
        }

        for (const auto& code : codes) {
            results.push_back({code, {response_result->success, response_result->message}});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [code, result] : results) {
            if (result.first) {
                BOOST_LOG_SEV(lg(), debug) << "Currency deleted: " << code;
                success_count++;
                emit self->currencyDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Currency deletion failed: " << code << " - " << result.second;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(result.second);
                }
            }
        }

        self->model_->load_page(self->paginationWidget_->current_offset(),
                                self->paginationWidget_->page_size());

        if (failure_count == 0) {
            QString msg = success_count == 1 ?
                              "Successfully deleted 1 currency" :
                              QString("Successfully deleted %1 currencies").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                              .arg(failure_count)
                              .arg(failure_count == 1 ? "currency" : "currencies")
                              .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg =
                QString("Deleted %1, failed to delete %2").arg(success_count).arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CurrencyMdiWindow::exportToCSV() {
    if (model_->rowCount() == 0) {
        QMessageBox::information(this, "No Data", "There are no currencies to export.");
        return;
    }

    auto currencies = model_->getCurrencies();

    QString fileName = QFileDialog::getSaveFileName(
        this, "Export to CSV", "currencies.csv", "CSV Files (*.csv);;All Files (*)");
    if (fileName.isEmpty())
        return;

    try {
        std::string csvData = refdata::csv::exporter::export_currency_config(currencies);

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            MessageBoxHelper::critical(
                this, "File Error", QString("Could not open file for writing: %1").arg(fileName));
            return;
        }
        file.write(csvData.c_str(), csvData.length());
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));
        emit statusChanged(QString("Successfully exported currencies to %1").arg(fileName));
    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, "Export Error", QString("Error during CSV export: %1").arg(e.what()));
    }
}

void CurrencyMdiWindow::exportToXML() {
    if (model_->rowCount() == 0) {
        QMessageBox::information(this, "No Data", "There are no currencies to export.");
        return;
    }

    auto currencies = model_->getCurrencies();

    QString fileName = QFileDialog::getSaveFileName(
        this, "Export to ORE XML", "currencies.xml", "XML Files (*.xml);;All Files (*)");
    if (fileName.isEmpty())
        return;

    try {
        std::string xmlData = ore::xml::exporter::export_currency_config(currencies);

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            MessageBoxHelper::critical(
                this, "File Error", QString("Could not open file for writing: %1").arg(fileName));
            return;
        }
        file.write(xmlData.c_str(), xmlData.length());
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));
        emit statusChanged(QString("Successfully exported currencies to %1").arg(fileName));
    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, "Export Error", QString("Error during XML export: %1").arg(e.what()));
    }
}

void CurrencyMdiWindow::importFromXML() {
    if (!clientManager_->isLoggedIn()) {
        MessageBoxHelper::warning(
            this, "Not Logged In", "Cannot import currencies while not logged in.");
        return;
    }

    QString fileName = QFileDialog::getOpenFileName(
        this, "Select ORE XML File to Import", QString(), "ORE XML Files (*.xml);;All Files (*)");
    if (fileName.isEmpty())
        return;

    emit statusChanged("Parsing XML file...");

    try {
        std::filesystem::path path(fileName.toStdString());
        auto currencies = ore::xml::importer::import_currency_config(path);

        if (currencies.empty()) {
            MessageBoxHelper::information(this,
                                          "No Currency Found",
                                          "The selected XML file does not contain any currencies.");
            emit statusChanged("Import cancelled - no currencies found");
            return;
        }

        emit statusChanged(
            QString("Found %1 currencies - opening import dialog...").arg(currencies.size()));

        std::vector<ImportEntityRow> rows;
        rows.reserve(currencies.size());
        for (const auto& currency : currencies) {
            const auto validation_error = ore::xml::importer::validate_currency(currency);
            rows.push_back({.display_values =
                                {
                                    QString::fromStdString(currency.iso_code),
                                    QString::fromStdString(currency.name),
                                    QString::fromStdString(currency.symbol),
                                    QString::fromStdString(currency.fraction_symbol),
                                    QString::number(currency.fractions_per_unit),
                                },
                            .is_valid = validation_error.empty(),
                            .invalid_reason = QString::fromStdString(validation_error)});
        }

        auto client_manager = clientManager_;
        auto username = username_;
        auto currencies_by_row = currencies;
        auto label_of = [currencies_by_row](std::size_t index) {
            return QString::fromStdString(currencies_by_row[index].iso_code);
        };
        auto import_one = [client_manager, username, currencies_by_row](std::size_t index) {
            auto currency_to_import = currencies_by_row[index];
            currency_to_import.modified_by = username.toStdString();
            try {
                auto request = refdata::messaging::save_currency_request::from(currency_to_import);
                auto response_result =
                    client_manager->process_authenticated_request(std::move(request));
                return response_result.has_value() && response_result->success;
            } catch (const std::exception&) {
                return false;
            }
        };

        auto* dialog = new ImportEntityDialog("currencies",
                                              fileName,
                                              {
                                                  "Code",
                                                  "Currency Name",
                                                  "Symbol",
                                                  "Fraction",
                                                  "Per Unit",
                                              },
                                              std::move(rows),
                                              label_of,
                                              import_one,
                                              this);

        connect(dialog,
                &ImportEntityDialog::importCompleted,
                this,
                [this](int success_count, int total_count) {
                    if (success_count > 0) {
                        paginationWidget_->reset_page();
                        model_->load_page(0, paginationWidget_->page_size());
                        QString message = QString("Successfully imported %1 of %2 currencies")
                                              .arg(success_count)
                                              .arg(total_count);
                        emit statusChanged(message);
                        MessageBoxHelper::information(this, "Import Complete", message);
                    } else {
                        emit statusChanged("Import failed - no currencies imported");
                        MessageBoxHelper::warning(
                            this,
                            "Import Failed",
                            "Failed to import currencies. Check the log for details.");
                    }
                });

        connect(dialog, &ImportEntityDialog::importCancelled, this, [this]() {
            emit statusChanged("Import cancelled");
        });

        if (dialog->exec() != QDialog::Accepted) {
            emit statusChanged("Import cancelled");
        }
        dialog->deleteLater();

    } catch (const std::exception& e) {
        MessageBoxHelper::critical(
            this, "Import Error", QString("Failed to import XML file:\n%1").arg(e.what()));
        emit statusChanged("Import failed");
    }
}

}
