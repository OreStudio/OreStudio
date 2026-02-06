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
#include "ores.qt/PublishDatasetsDialog.hpp"

#include <algorithm>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QTimer>
#include <QApplication>
#include <QTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// PublishDatasetsDialog (Main Wizard)
// ============================================================================

PublishDatasetsDialog::PublishDatasetsDialog(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager),
      username_(username) {

    setWindowTitle(tr("Publish Datasets"));
    setMinimumSize(600, 500);
    resize(700, 550);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void PublishDatasetsDialog::setupPages() {
    setPage(Page_Selection, new SelectionPage(this));
    setPage(Page_Options, new OptionsPage(this));
    setPage(Page_Review, new ReviewPage(this));
    setPage(Page_Progress, new ProgressPage(this));
    setPage(Page_Results, new ResultsPage(this));

    setStartId(Page_Selection);
}

void PublishDatasetsDialog::setDatasets(
    const std::vector<dq::domain::dataset>& datasets) {

    datasets_ = datasets;

    // Store the requested IDs
    requestedIds_.clear();
    for (const auto& ds : datasets_) {
        requestedIds_.push_back(ds.id);
    }
}

dq::domain::publication_mode PublishDatasetsDialog::selectedMode() const {
    auto* optionsPage = qobject_cast<OptionsPage*>(page(Page_Options));
    if (!optionsPage) {
        return dq::domain::publication_mode::upsert;
    }

    auto modeStr = optionsPage->modeCombo()->currentData().toString().toStdString();
    auto mode = dq::domain::publication_mode_from_string(modeStr);
    return mode.value_or(dq::domain::publication_mode::upsert);
}

bool PublishDatasetsDialog::resolveDependencies() const {
    auto* optionsPage = qobject_cast<OptionsPage*>(page(Page_Options));
    if (!optionsPage) {
        return true;
    }
    return optionsPage->resolveDependenciesCheck()->isChecked();
}

// ============================================================================
// SelectionPage
// ============================================================================

SelectionPage::SelectionPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Selected Datasets"));
    setSubTitle(tr("Review the datasets you have selected for publication."));

    auto* layout = new QVBoxLayout(this);

    countLabel_ = new QLabel(this);
    countLabel_->setStyleSheet("font-weight: bold; font-size: 14px;");
    layout->addWidget(countLabel_);

    layout->addSpacing(10);

    datasetList_ = new QListWidget(this);
    datasetList_->setSelectionMode(QAbstractItemView::NoSelection);
    datasetList_->setAlternatingRowColors(true);
    layout->addWidget(datasetList_, 1);
}

void SelectionPage::initializePage() {
    datasetList_->clear();

    const auto& datasets = wizard_->datasets();
    for (const auto& ds : datasets) {
        auto* item = new QListWidgetItem(
            QString::fromStdString(ds.code) + " - " +
            QString::fromStdString(ds.name));
        datasetList_->addItem(item);
    }

    countLabel_->setText(tr("%1 dataset(s) selected for publication")
        .arg(datasets.size()));
}

// ============================================================================
// OptionsPage
// ============================================================================

OptionsPage::OptionsPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publication Options"));
    setSubTitle(tr("Configure how the datasets should be published."));

    auto* layout = new QFormLayout(this);
    layout->setSpacing(16);

    // Publication mode
    modeCombo_ = new QComboBox(this);
    modeCombo_->addItem(tr("Upsert (insert new, update existing)"), "upsert");
    modeCombo_->addItem(tr("Insert Only (skip existing)"), "insert_only");
    modeCombo_->addItem(tr("Replace All (delete then insert)"), "replace_all");
    layout->addRow(tr("Publication Mode:"), modeCombo_);

    // Mode description
    auto* modeDescription = new QLabel(
        tr("<i>Upsert</i> will insert new records and update existing ones.<br>"
           "<i>Insert Only</i> will only add new records, skipping existing.<br>"
           "<i>Replace All</i> will delete all existing records first, then insert."),
        this);
    modeDescription->setWordWrap(true);
    layout->addRow("", modeDescription);

    // Dependencies
    resolveDependenciesCheck_ = new QCheckBox(
        tr("Automatically publish dependencies first"), this);
    resolveDependenciesCheck_->setChecked(true);
    layout->addRow(tr("Dependencies:"), resolveDependenciesCheck_);

    auto* dependencyDescription = new QLabel(
        tr("When enabled, any datasets that this dataset depends on will be "
           "published first in the correct order."),
        this);
    dependencyDescription->setWordWrap(true);
    layout->addRow("", dependencyDescription);

    // Register fields for access from other pages
    registerField("publicationMode", modeCombo_);
    registerField("resolveDependencies", resolveDependenciesCheck_);
}

// ============================================================================
// ReviewPage
// ============================================================================

ReviewPage::ReviewPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Review Publication Order"));
    setSubTitle(tr("Verify the order in which datasets will be published."));

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(this);
    layout->addWidget(statusLabel_);

    layout->addSpacing(10);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(summaryLabel_);

    layout->addSpacing(10);

    orderTable_ = new QTableWidget(this);
    orderTable_->setColumnCount(3);
    orderTable_->setHorizontalHeaderLabels({
        tr("Order"),
        tr("Dataset Code"),
        tr("Type")
    });
    orderTable_->horizontalHeader()->setStretchLastSection(true);
    orderTable_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    orderTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    orderTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    orderTable_->setAlternatingRowColors(true);
    layout->addWidget(orderTable_, 1);
}

void ReviewPage::initializePage() {
    resolved_ = false;
    statusLabel_->setText(tr("Resolving dependencies..."));
    orderTable_->setRowCount(0);
    summaryLabel_->clear();

    // Resolve dependencies after a short delay to show the UI
    QTimer::singleShot(100, this, &ReviewPage::resolveDependencies);
}

bool ReviewPage::isComplete() const {
    return resolved_;
}

void ReviewPage::resolveDependencies() {
    bool resolve = wizard_->resolveDependencies();

    if (!resolve) {
        // No dependency resolution - just use the selected datasets
        wizard_->resolvedDatasets() = wizard_->datasets();
        wizard_->requestedIds().clear();
        for (const auto& ds : wizard_->datasets()) {
            wizard_->requestedIds().push_back(ds.id);
        }
        resolved_ = true;
        statusLabel_->setText(tr("Ready to publish."));
        updatePublicationOrder();
        emit completeChanged();
        return;
    }

    // Build request
    dq::messaging::resolve_dependencies_request request;
    for (const auto& ds : wizard_->datasets()) {
        request.dataset_ids.push_back(ds.id);
    }

    // Send request
    auto result = wizard_->clientManager()->process_request(std::move(request));

    if (!result) {
        // Fall back to just the selected datasets
        wizard_->resolvedDatasets() = wizard_->datasets();
        wizard_->requestedIds().clear();
        for (const auto& ds : wizard_->datasets()) {
            wizard_->requestedIds().push_back(ds.id);
        }
        resolved_ = true;
        // Include the actual error message from the server
        QString errorDetail = result.error().message.empty()
            ? tr("unknown error")
            : QString::fromStdString(result.error().message);
        statusLabel_->setText(tr("Failed to resolve dependencies (%1). Will publish selected datasets only.")
            .arg(errorDetail));
        updatePublicationOrder();
        emit completeChanged();
        return;
    }

    wizard_->resolvedDatasets() = std::move(result->datasets);
    wizard_->requestedIds() = std::move(result->requested_ids);

    resolved_ = true;
    statusLabel_->setText(tr("Dependencies resolved successfully."));
    updatePublicationOrder();
    emit completeChanged();
}

void ReviewPage::updatePublicationOrder() {
    const auto& resolvedDatasets = wizard_->resolvedDatasets();
    const auto& requestedIds = wizard_->requestedIds();

    orderTable_->setRowCount(static_cast<int>(resolvedDatasets.size()));

    int dependencyCount = 0;
    int requestedCount = 0;

    for (int row = 0; row < static_cast<int>(resolvedDatasets.size()); ++row) {
        const auto& ds = resolvedDatasets[row];

        // Order column
        auto* orderItem = new QTableWidgetItem(QString::number(row + 1));
        orderItem->setTextAlignment(Qt::AlignCenter);
        orderTable_->setItem(row, 0, orderItem);

        // Code column
        auto* codeItem = new QTableWidgetItem(
            QString::fromStdString(ds.code));
        orderTable_->setItem(row, 1, codeItem);

        // Type column - check if this is a dependency or requested
        bool isRequested = std::ranges::find(requestedIds, ds.id) != requestedIds.end();
        QString typeText = isRequested ? tr("Selected") : tr("Dependency");
        auto* typeItem = new QTableWidgetItem(typeText);

        if (!isRequested) {
            // Use italic for dependencies - visible in both light and dark themes
            QFont font = typeItem->font();
            font.setItalic(true);
            typeItem->setFont(font);
            ++dependencyCount;
        } else {
            ++requestedCount;
        }
        orderTable_->setItem(row, 2, typeItem);
    }

    // Update summary label
    if (dependencyCount > 0) {
        summaryLabel_->setText(
            tr("%1 dataset(s) will be published: %2 selected, %3 dependencies")
                .arg(resolvedDatasets.size())
                .arg(requestedCount)
                .arg(dependencyCount));
    } else {
        summaryLabel_->setText(
            tr("%1 dataset(s) will be published")
                .arg(resolvedDatasets.size()));
    }
}

// ============================================================================
// ProgressPage
// ============================================================================

ProgressPage::ProgressPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publishing"));
    setSubTitle(tr("Please wait while the datasets are being published."));

    auto* layout = new QVBoxLayout(this);
    layout->addStretch();

    statusLabel_ = new QLabel(tr("Starting publication..."), this);
    statusLabel_->setAlignment(Qt::AlignCenter);
    statusLabel_->setStyleSheet("font-size: 14px;");
    layout->addWidget(statusLabel_);

    layout->addSpacing(20);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);  // Indeterminate
    progressBar_->setMinimumWidth(400);
    layout->addWidget(progressBar_, 0, Qt::AlignCenter);

    layout->addSpacing(10);

    currentDatasetLabel_ = new QLabel(this);
    currentDatasetLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(currentDatasetLabel_);

    layout->addStretch();

    // Hide back/next buttons while publishing
    setCommitPage(true);
}

void ProgressPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    statusLabel_->setText(tr("Starting publication..."));
    currentDatasetLabel_->clear();
    wizard_->lastError().clear();  // Clear any previous error

    // Start publishing after a short delay
    QTimer::singleShot(100, this, &ProgressPage::performPublish);
}

bool ProgressPage::isComplete() const {
    return publishComplete_;
}

int ProgressPage::nextId() const {
    return PublishDatasetsDialog::Page_Results;
}

void ProgressPage::performPublish() {
    const auto& resolvedDatasets = wizard_->resolvedDatasets();

    if (resolvedDatasets.empty()) {
        statusLabel_->setText(tr("No datasets to publish."));
        publishComplete_ = true;
        publishSuccess_ = false;
        emit completeChanged();
        wizard_->next();
        return;
    }

    // Update status
    statusLabel_->setText(tr("Publishing %1 dataset(s)...")
        .arg(resolvedDatasets.size()));
    currentDatasetLabel_->setText(tr("Sending request to server..."));
    QApplication::processEvents();

    // Build request
    dq::messaging::publish_datasets_request request;
    for (const auto& ds : resolvedDatasets) {
        request.dataset_ids.push_back(ds.id);
    }
    request.mode = wizard_->selectedMode();
    request.published_by = wizard_->username().toStdString();
    request.resolve_dependencies = false;  // Already resolved

    // Send request
    auto result = wizard_->clientManager()->process_request(std::move(request));

    if (!result) {
        statusLabel_->setText(tr("Publication failed!"));
        // Extract and display the actual error message from the server
        QString errorMsg = result.error().message.empty()
            ? tr("Failed to communicate with server.")
            : QString::fromStdString(result.error().message);
        currentDatasetLabel_->setText(errorMsg);
        // Store error for ResultsPage to display
        wizard_->lastError() = errorMsg;
        publishComplete_ = true;
        publishSuccess_ = false;
        emit completeChanged();
        wizard_->next();
        return;
    }

    // Store results
    wizard_->results() = std::move(result->results);

    statusLabel_->setText(tr("Publication complete!"));
    currentDatasetLabel_->clear();
    publishComplete_ = true;
    publishSuccess_ = true;
    emit completeChanged();

    // Notify that datasets were published (for cache refresh)
    // Collect the codes of published datasets
    QStringList publishedCodes;
    for (const auto& ds : resolvedDatasets) {
        publishedCodes.append(QString::fromStdString(ds.code));
    }
    emit wizard_->datasetsPublished(publishedCodes);

    // Auto-advance to results
    wizard_->next();
}

// ============================================================================
// ResultsPage
// ============================================================================

ResultsPage::ResultsPage(PublishDatasetsDialog* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publication Results"));
    setSubTitle(tr("Summary of the publication operation."));
    setFinalPage(true);

    auto* layout = new QVBoxLayout(this);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setStyleSheet("font-weight: bold; font-size: 14px;");
    summaryLabel_->setWordWrap(true);
    layout->addWidget(summaryLabel_);

    layout->addSpacing(10);

    // Log-style output for publication results
    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setMinimumHeight(200);
    logOutput_->setStyleSheet(
        "QTextEdit { background-color: #1e1e1e; color: #d4d4d4; "
        "font-family: monospace; font-size: 11px; }");
    layout->addWidget(logOutput_, 1);
}

void ResultsPage::initializePage() {
    logOutput_->clear();
    const auto& results = wizard_->results();
    const QString& lastError = wizard_->lastError();

    std::uint64_t totalInserted = 0;
    std::uint64_t totalUpdated = 0;
    std::uint64_t totalSkipped = 0;
    std::uint64_t totalDeleted = 0;
    int successCount = 0;
    int failureCount = 0;

    // Check for publication error first
    if (!lastError.isEmpty()) {
        appendLog(tr("=== Publication Failed ==="));
        appendError(tr("Publication error: %1").arg(lastError));
        summaryLabel_->setText(tr("Publication failed due to an error."));
        summaryLabel_->setStyleSheet("font-weight: bold; font-size: 14px; color: #dc3545;");
        return;
    }

    if (results.empty()) {
        appendLog(tr("No datasets were published."));
        summaryLabel_->setText(tr("No datasets were published."));
        summaryLabel_->setStyleSheet("font-weight: bold; font-size: 14px;");
        return;
    }

    // Log each dataset result
    appendLog(tr("=== Publication Results ==="));
    appendLog("");

    for (const auto& r : results) {
        QString datasetCode = QString::fromStdString(r.dataset_code);
        QString targetTable = QString::fromStdString(r.target_table);

        if (r.success) {
            ++successCount;
            totalInserted += r.records_inserted;
            totalUpdated += r.records_updated;
            totalSkipped += r.records_skipped;
            totalDeleted += r.records_deleted;

            appendSuccess(tr("[SUCCESS] %1").arg(datasetCode));
            appendLog(tr("  Target: %1").arg(targetTable));
            appendLog(tr("  Records: %1 inserted, %2 updated, %3 skipped, %4 deleted")
                .arg(r.records_inserted)
                .arg(r.records_updated)
                .arg(r.records_skipped)
                .arg(r.records_deleted));
        } else {
            ++failureCount;
            appendError(tr("[FAILED] %1").arg(datasetCode));
            appendLog(tr("  Target: %1").arg(targetTable));
            if (!r.error_message.empty()) {
                appendError(tr("  Error: %1").arg(QString::fromStdString(r.error_message)));
            }
        }
        appendLog("");
    }

    // Log summary
    appendLog(tr("=== Summary ==="));
    appendLog(tr("Datasets processed: %1").arg(results.size()));
    appendLog(tr("Succeeded: %1, Failed: %2").arg(successCount).arg(failureCount));
    appendLog(tr("Total records: %1 inserted, %2 updated, %3 skipped, %4 deleted")
        .arg(totalInserted)
        .arg(totalUpdated)
        .arg(totalSkipped)
        .arg(totalDeleted));

    // Update summary label
    QString summary;
    if (failureCount == 0) {
        summary = tr("Successfully published %1 dataset(s).").arg(successCount);
        summaryLabel_->setStyleSheet("font-weight: bold; font-size: 14px; color: #28a745;");
    } else if (successCount == 0) {
        summary = tr("Publication failed for all %1 dataset(s).").arg(failureCount);
        summaryLabel_->setStyleSheet("font-weight: bold; font-size: 14px; color: #dc3545;");
    } else {
        summary = tr("Published %1 of %2 dataset(s) (%3 failed).")
            .arg(successCount)
            .arg(results.size())
            .arg(failureCount);
        summaryLabel_->setStyleSheet("font-weight: bold; font-size: 14px; color: #ffc107;");
    }
    summaryLabel_->setText(summary);
}

void ResultsPage::appendLog(const QString& message) {
    logOutput_->append(message);
    // Scroll to bottom
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ResultsPage::appendError(const QString& message) {
    // Use HTML for red error text
    logOutput_->append(QString("<span style='color: #ff6b6b;'>%1</span>").arg(message.toHtmlEscaped()));
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ResultsPage::appendSuccess(const QString& message) {
    // Use HTML for green success text
    logOutput_->append(QString("<span style='color: #69db7c;'>%1</span>").arg(message.toHtmlEscaped()));
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

}
