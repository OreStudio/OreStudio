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
#include "ores.qt/PublishBundleWizard.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QMessageBox>
#include <QTimer>
#include <QHeaderView>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/LeiEntityPicker.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"
#include "ores.dq/messaging/dataset_bundle_member_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// PublishBundleWizard (Main Wizard)
// ============================================================================

PublishBundleWizard::PublishBundleWizard(
    ClientManager* clientManager,
    const QString& bundleCode,
    const QString& bundleName,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager),
      bundleCode_(bundleCode),
      bundleName_(bundleName) {

    setWindowTitle(tr("Publish Bundle - %1").arg(bundleName));
    setMinimumSize(700, 550);
    resize(800, 600);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void PublishBundleWizard::setupPages() {
    setPage(Page_BundleSummary, new BundleSummaryPage(this));
    setPage(Page_OptionalDatasets, new OptionalDatasetsPage(this));
    setPage(Page_LeiPartyConfig, new LeiPartyConfigPage(this));
    setPage(Page_Confirm, new ConfirmPublishPage(this));
    setPage(Page_Progress, new PublishProgressPage(this));
    setPage(Page_Results, new PublishResultsPage(this));

    setStartId(Page_BundleSummary);
}

// ============================================================================
// BundleSummaryPage
// ============================================================================

BundleSummaryPage::BundleSummaryPage(PublishBundleWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Bundle Summary"));
    setSubTitle(tr("Review the datasets in this bundle before publishing."));

    setupUI();
}

void BundleSummaryPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Bundle name header
    bundleNameLabel_ = new QLabel(this);
    bundleNameLabel_->setStyleSheet("font-size: 14pt; font-weight: bold;");
    layout->addWidget(bundleNameLabel_);

    layout->addSpacing(10);

    // Members table
    membersModel_ = new QStandardItemModel(0, 4, this);
    membersModel_->setHorizontalHeaderLabels({
        tr("Order"), tr("Dataset"), tr("Dataset Code"), tr("Optional")
    });

    membersTable_ = new QTableView(this);
    membersTable_->setModel(membersModel_);
    membersTable_->setSelectionMode(QAbstractItemView::NoSelection);
    membersTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    membersTable_->horizontalHeader()->setStretchLastSection(true);
    membersTable_->verticalHeader()->setVisible(false);
    membersTable_->setAlternatingRowColors(true);
    layout->addWidget(membersTable_, 1);

    // Status label
    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);
}

void BundleSummaryPage::initializePage() {
    bundleNameLabel_->setText(wizard_->bundleName());

    if (!membersLoaded_) {
        loadMembers();
    }
}

void BundleSummaryPage::loadMembers() {
    BOOST_LOG_SEV(lg(), info) << "Loading bundle members for: "
                              << wizard_->bundleCode().toStdString();

    statusLabel_->setText(tr("Loading bundle datasets..."));

    dq::messaging::get_dataset_bundle_members_by_bundle_request request;
    request.bundle_code = wizard_->bundleCode().toStdString();

    auto result = wizard_->clientManager()->process_request(std::move(request));

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load bundle members.";
        statusLabel_->setText(
            tr("Failed to load bundle datasets. Please check your connection "
               "and try again."));
        return;
    }

    // Store members in wizard for use by other pages
    wizard_->setMembers(std::move(result->members));
    membersLoaded_ = true;

    // Scan for lei_parties datasets and optional members
    bool hasLeiParties = false;
    bool hasOptional = false;
    for (const auto& member : wizard_->members()) {
        if (member.dataset_code.find("lei_parties") != std::string::npos) {
            hasLeiParties = true;
        }
        if (member.optional) {
            hasOptional = true;
        }
    }
    wizard_->setNeedsLeiPartyConfig(hasLeiParties);
    wizard_->setHasOptionalDatasets(hasOptional);

    if (hasLeiParties) {
        BOOST_LOG_SEV(lg(), info)
            << "Bundle contains lei_parties dataset; LEI configuration required.";
    }
    if (hasOptional) {
        BOOST_LOG_SEV(lg(), info)
            << "Bundle contains optional datasets.";
    }

    populateTable();

    const auto count = wizard_->members().size();
    statusLabel_->setText(
        tr("Found %1 dataset(s) in this bundle.").arg(count));
}

void BundleSummaryPage::populateTable() {
    membersModel_->removeRows(0, membersModel_->rowCount());

    for (const auto& member : wizard_->members()) {
        const int row = membersModel_->rowCount();
        membersModel_->insertRow(row);

        auto* orderItem = new QStandardItem(
            QString::number(member.display_order));
        orderItem->setTextAlignment(Qt::AlignCenter);
        membersModel_->setItem(row, 0, orderItem);

        // Use dataset_code as the display name (no separate name field)
        membersModel_->setItem(row, 1, new QStandardItem(
            QString::fromStdString(member.dataset_code)));

        membersModel_->setItem(row, 2, new QStandardItem(
            QString::fromStdString(member.dataset_code)));

        auto* optionalItem = new QStandardItem(
            member.optional ? tr("Yes") : tr("No"));
        optionalItem->setTextAlignment(Qt::AlignCenter);
        membersModel_->setItem(row, 3, optionalItem);
    }

    // Resize columns
    membersTable_->resizeColumnsToContents();
    membersTable_->setColumnWidth(0, 60);
    membersTable_->setColumnWidth(3, 70);
}

int BundleSummaryPage::nextId() const {
    if (wizard_->hasOptionalDatasets()) {
        return PublishBundleWizard::Page_OptionalDatasets;
    }
    if (wizard_->needsLeiPartyConfig()) {
        return PublishBundleWizard::Page_LeiPartyConfig;
    }
    return PublishBundleWizard::Page_Confirm;
}

// ============================================================================
// OptionalDatasetsPage
// ============================================================================

OptionalDatasetsPage::OptionalDatasetsPage(PublishBundleWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Optional Datasets"));
    setSubTitle(tr("Select which optional datasets to include in this "
                   "publication."));

    setupUI();
}

void OptionalDatasetsPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    auto* infoLabel = new QLabel(
        tr("The following datasets are optional. Check the ones you want to "
           "publish. Unchecked datasets will be skipped."), this);
    infoLabel->setWordWrap(true);
    layout->addWidget(infoLabel);

    layout->addSpacing(10);

    checkboxLayout_ = new QVBoxLayout();
    layout->addLayout(checkboxLayout_);

    layout->addStretch();
}

void OptionalDatasetsPage::initializePage() {
    // Clear previous checkboxes
    for (auto* cb : checkboxes_) {
        checkboxLayout_->removeWidget(cb);
        delete cb;
    }
    checkboxes_.clear();

    // Create a checkbox for each optional member
    for (const auto& member : wizard_->members()) {
        if (!member.optional) continue;

        auto* cb = new QCheckBox(
            QString::fromStdString(member.dataset_code), this);

        // Counterparty datasets are disabled (Phase 2)
        if (member.dataset_code.find("counterpart") != std::string::npos) {
            cb->setEnabled(false);
            cb->setChecked(false);
            cb->setToolTip(tr("Coming soon - requires party-scoped "
                              "counterparty migration."));
        }

        checkboxLayout_->addWidget(cb);
        checkboxes_.push_back(cb);
    }
}

bool OptionalDatasetsPage::validatePage() {
    QSet<QString> opted;
    for (auto* cb : checkboxes_) {
        if (cb->isChecked()) {
            opted.insert(cb->text());
        }
    }
    wizard_->setOptedInDatasets(opted);

    // Update needsLeiPartyConfig based on whether lei_parties is opted in
    bool leiOptedIn = false;
    for (const auto& ds : opted) {
        if (ds.contains("lei_parties")) {
            leiOptedIn = true;
            break;
        }
    }
    wizard_->setNeedsLeiPartyConfig(leiOptedIn);

    return true;
}

int OptionalDatasetsPage::nextId() const {
    // Check if lei_parties is currently checked (for dynamic nextId)
    for (auto* cb : checkboxes_) {
        if (cb->isChecked() && cb->text().contains("lei_parties")) {
            return PublishBundleWizard::Page_LeiPartyConfig;
        }
    }
    return PublishBundleWizard::Page_Confirm;
}

// ============================================================================
// LeiPartyConfigPage
// ============================================================================

LeiPartyConfigPage::LeiPartyConfigPage(PublishBundleWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("LEI Entity Configuration"));
    setSubTitle(tr("This bundle contains an LEI parties dataset. Select the "
                   "root LEI entity to use as the starting point for party "
                   "data retrieval."));

    setupUI();
}

void LeiPartyConfigPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Instruction text
    instructionLabel_ = new QLabel(this);
    instructionLabel_->setWordWrap(true);
    instructionLabel_->setText(
        tr("Use the search below to find and select the LEI entity that will "
           "serve as the root for the GLEIF relationship tree. All direct and "
           "indirect children of this entity will be included in the party "
           "import."));
    layout->addWidget(instructionLabel_);

    layout->addSpacing(10);

    // LEI entity picker
    leiPicker_ = new LeiEntityPicker(wizard_->clientManager(), this);
    layout->addWidget(leiPicker_, 1);

    layout->addSpacing(10);

    // Selected entity display
    auto* selectionBox = new QGroupBox(tr("Selected Entity"), this);
    auto* selectionLayout = new QVBoxLayout(selectionBox);
    selectedEntityLabel_ = new QLabel(tr("No entity selected."), this);
    selectedEntityLabel_->setWordWrap(true);
    selectionLayout->addWidget(selectedEntityLabel_);
    layout->addWidget(selectionBox);

    // Connect entity selection signal
    connect(leiPicker_, &LeiEntityPicker::entitySelected,
            this, [this](const QString& lei, const QString& name) {
        wizard_->setRootLei(lei);
        wizard_->setRootLeiName(name);
        selectedEntityLabel_->setText(
            tr("<b>%1</b> - %2").arg(lei, name));
        emit completeChanged();
    });
}

void LeiPartyConfigPage::initializePage() {
    // Load LEI entities on first visit
    if (!leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }

    // Reset selection display if returning to this page
    if (wizard_->rootLei().isEmpty()) {
        selectedEntityLabel_->setText(tr("No entity selected."));
    } else {
        selectedEntityLabel_->setText(
            tr("<b>%1</b> - %2").arg(
                wizard_->rootLei(), wizard_->rootLeiName()));
    }
}

bool LeiPartyConfigPage::validatePage() {
    if (wizard_->rootLei().isEmpty()) {
        QMessageBox::warning(this, tr("Entity Required"),
            tr("Please select a root LEI entity before continuing."));
        return false;
    }
    return true;
}

int LeiPartyConfigPage::nextId() const {
    return PublishBundleWizard::Page_Confirm;
}

// ============================================================================
// ConfirmPublishPage
// ============================================================================

ConfirmPublishPage::ConfirmPublishPage(PublishBundleWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Confirm Publication"));
    setSubTitle(tr("Review the publication settings and click 'Next' to begin "
                   "publishing."));

    setupUI();
}

void ConfirmPublishPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Summary label
    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    summaryLabel_->setTextFormat(Qt::RichText);
    layout->addWidget(summaryLabel_);

    layout->addSpacing(20);

    // Publication settings
    auto* settingsBox = new QGroupBox(tr("Publication Settings"), this);
    auto* settingsLayout = new QFormLayout(settingsBox);

    // Mode dropdown
    modeCombo_ = new QComboBox(this);
    modeCombo_->addItem(tr("Upsert (insert new, update existing)"), 0);
    modeCombo_->addItem(tr("Insert Only (skip existing)"), 1);
    modeCombo_->addItem(tr("Replace All (delete all, then insert)"), 2);
    modeCombo_->setCurrentIndex(0);
    settingsLayout->addRow(tr("Mode:"), modeCombo_);

    // Atomic checkbox
    atomicCheckbox_ = new QCheckBox(
        tr("Atomic (all datasets succeed or all fail)"), this);
    atomicCheckbox_->setChecked(true);
    settingsLayout->addRow(tr("Transaction:"), atomicCheckbox_);

    layout->addWidget(settingsBox);
    layout->addStretch();
}

void ConfirmPublishPage::initializePage() {
    const auto memberCount = wizard_->members().size();

    QString summary = tr(
        "<p><b>Bundle:</b> %1</p>"
        "<p><b>Datasets:</b> %2</p>")
        .arg(wizard_->bundleName())
        .arg(memberCount);

    if (wizard_->needsLeiPartyConfig() && !wizard_->rootLei().isEmpty()) {
        summary += tr(
            "<p><b>Root LEI:</b> %1</p>"
            "<p><b>Entity:</b> %2</p>")
            .arg(wizard_->rootLei(), wizard_->rootLeiName());
    }

    summaryLabel_->setText(summary);

    // Update button label
    wizard()->setButtonText(QWizard::NextButton, tr("Publish"));
}

bool ConfirmPublishPage::validatePage() {
    // Store selected mode
    const int modeIndex = modeCombo_->currentIndex();
    switch (modeIndex) {
    case 0:
        wizard_->setSelectedMode(dq::domain::publication_mode::upsert);
        break;
    case 1:
        wizard_->setSelectedMode(dq::domain::publication_mode::insert_only);
        break;
    case 2:
        wizard_->setSelectedMode(dq::domain::publication_mode::replace_all);
        break;
    default:
        wizard_->setSelectedMode(dq::domain::publication_mode::upsert);
        break;
    }

    wizard_->setAtomic(atomicCheckbox_->isChecked());
    return true;
}

int ConfirmPublishPage::nextId() const {
    return PublishBundleWizard::Page_Progress;
}

// ============================================================================
// PublishProgressPage
// ============================================================================

PublishProgressPage::PublishProgressPage(PublishBundleWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publishing"));
    setSubTitle(tr("Please wait while the bundle is being published."));
    setFinalPage(false);
    setCommitPage(true);

    auto* layout = new QVBoxLayout(this);

    // Status label
    statusLabel_ = new QLabel(tr("Preparing to publish..."), this);
    statusLabel_->setAlignment(Qt::AlignCenter);
    statusLabel_->setStyleSheet("font-size: 14px; font-weight: bold;");
    layout->addWidget(statusLabel_);

    layout->addSpacing(20);

    // Progress bar (indeterminate)
    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);
    progressBar_->setMinimumWidth(400);
    progressBar_->setTextVisible(false);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");
    layout->addWidget(progressBar_, 0, Qt::AlignCenter);

    layout->addStretch();
}

void PublishProgressPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    progressBar_->setRange(0, 0);
    statusLabel_->setText(tr("Publishing bundle..."));

    // Disable navigation during publishing
    wizard()->button(QWizard::FinishButton)->setEnabled(false);
    wizard()->button(QWizard::BackButton)->setVisible(false);

    // Start publishing after a short delay
    QTimer::singleShot(100, this, &PublishProgressPage::startPublish);
}

bool PublishProgressPage::isComplete() const {
    return publishComplete_;
}

int PublishProgressPage::nextId() const {
    return PublishBundleWizard::Page_Results;
}

void PublishProgressPage::startPublish() {
    BOOST_LOG_SEV(lg(), info) << "Starting bundle publication (async): "
                              << wizard_->bundleCode().toStdString();

    statusLabel_->setText(tr("Publishing bundle '%1'...").arg(
        wizard_->bundleName()));

    // Capture values needed for the background thread
    const std::string bundleCode = wizard_->bundleCode().toStdString();
    const dq::domain::publication_mode mode = wizard_->selectedMode();
    const bool atomic = wizard_->isAtomic();
    const std::string rootLei = wizard_->rootLei().toStdString();
    const bool needsLei = wizard_->needsLeiPartyConfig();
    ClientManager* clientManager = wizard_->clientManager();

    // Get published_by from current session
    const std::string publishedBy = clientManager->currentUsername();

    // Build opted_in_datasets array
    const QSet<QString>& optedIn = wizard_->optedInDatasets();
    std::string optedInJson = "[";
    bool first = true;
    for (const auto& ds : optedIn) {
        if (!first) optedInJson += ",";
        optedInJson += "\"" + ds.toStdString() + "\"";
        first = false;
    }
    optedInJson += "]";

    // Build params_json
    std::string paramsJson = "{\"opted_in_datasets\":" + optedInJson;
    if (needsLei && !rootLei.empty()) {
        paramsJson += ",\"lei_parties\":{\"root_lei\":\"" + rootLei + "\"}";
    }
    paramsJson += "}";

    using ResponseType = dq::messaging::publish_bundle_response;

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        // Stop indeterminate animation
        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        auto* resultsPage = qobject_cast<PublishResultsPage*>(
            wizard()->page(PublishBundleWizard::Page_Results));

        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to communicate with server "
                                       << "for bundle publication.";
            statusLabel_->setText(tr("Publication failed!"));
            progressBar_->setStyleSheet(
                "QProgressBar { border: 1px solid #8B0000; border-radius: 3px; "
                "background: #2d2d2d; }"
                "QProgressBar::chunk { background-color: #CD5C5C; }");

            publishComplete_ = true;
            publishSuccess_ = false;

            if (resultsPage) {
                resultsPage->setResults(false,
                    tr("Failed to communicate with server."), {});
            }
        } else if (!result->success) {
            BOOST_LOG_SEV(lg(), error) << "Bundle publication failed: "
                                       << result->error_message;
            statusLabel_->setText(tr("Publication failed!"));
            progressBar_->setStyleSheet(
                "QProgressBar { border: 1px solid #8B0000; border-radius: 3px; "
                "background: #2d2d2d; }"
                "QProgressBar::chunk { background-color: #CD5C5C; }");

            publishComplete_ = true;
            publishSuccess_ = false;

            if (resultsPage) {
                resultsPage->setResults(false,
                    QString::fromStdString(result->error_message),
                    result->dataset_results);
            }
        } else {
            BOOST_LOG_SEV(lg(), info) << "Bundle publication succeeded: "
                << result->datasets_succeeded << " datasets, "
                << result->total_records_inserted << " inserted, "
                << result->total_records_updated << " updated.";

            statusLabel_->setText(tr("Publication completed successfully!"));
            publishComplete_ = true;
            publishSuccess_ = true;

            if (resultsPage) {
                resultsPage->setResults(true, QString(),
                    result->dataset_results);
            }

            emit wizard_->bundlePublished(wizard_->bundleCode());
        }

        emit completeChanged();

        // Auto-advance to results page
        if (publishComplete_) {
            wizard()->next();
        }
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, bundleCode, mode, publishedBy, atomic,
         paramsJson]() -> std::optional<ResponseType> {

            dq::messaging::publish_bundle_request request;
            request.bundle_code = bundleCode;
            request.mode = mode;
            request.published_by = publishedBy;
            request.atomic = atomic;
            request.params_json = paramsJson;

            auto result = clientManager->process_authenticated_request(
                std::move(request));

            if (!result) {
                return std::nullopt;
            }
            return *result;
        }
    );

    watcher->setFuture(future);
}

// ============================================================================
// PublishResultsPage
// ============================================================================

PublishResultsPage::PublishResultsPage(PublishBundleWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publication Results"));
    setSubTitle(tr("Summary of the bundle publication."));
    setFinalPage(true);

    setupUI();
}

void PublishResultsPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Overall status
    overallStatusLabel_ = new QLabel(this);
    overallStatusLabel_->setWordWrap(true);
    overallStatusLabel_->setStyleSheet("font-size: 14px; font-weight: bold;");
    layout->addWidget(overallStatusLabel_);

    layout->addSpacing(10);

    // Results table
    resultsModel_ = new QStandardItemModel(0, 7, this);
    resultsModel_->setHorizontalHeaderLabels({
        tr("Dataset"), tr("Status"), tr("Inserted"), tr("Updated"),
        tr("Skipped"), tr("Deleted"), tr("Error")
    });

    resultsTable_ = new QTableView(this);
    resultsTable_->setModel(resultsModel_);
    resultsTable_->setSelectionMode(QAbstractItemView::NoSelection);
    resultsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    resultsTable_->horizontalHeader()->setStretchLastSection(true);
    resultsTable_->verticalHeader()->setVisible(false);
    resultsTable_->setAlternatingRowColors(true);
    layout->addWidget(resultsTable_, 1);
}

void PublishResultsPage::setResults(
    bool overallSuccess,
    const QString& errorMessage,
    const std::vector<dq::messaging::bundle_dataset_result>& results) {

    overallSuccess_ = overallSuccess;
    errorMessage_ = errorMessage;
    datasetResults_ = results;
}

void PublishResultsPage::initializePage() {
    if (overallSuccess_) {
        overallStatusLabel_->setText(tr("Publication completed successfully."));
        overallStatusLabel_->setStyleSheet(
            "font-size: 14px; font-weight: bold; color: #228B22;");
    } else {
        QString msg = tr("Publication failed.");
        if (!errorMessage_.isEmpty()) {
            msg += " " + errorMessage_;
        }
        overallStatusLabel_->setText(msg);
        overallStatusLabel_->setStyleSheet(
            "font-size: 14px; font-weight: bold; color: #cc0000;");
    }

    populateResults();
}

void PublishResultsPage::populateResults() {
    resultsModel_->removeRows(0, resultsModel_->rowCount());

    for (const auto& r : datasetResults_) {
        const int row = resultsModel_->rowCount();
        resultsModel_->insertRow(row);

        resultsModel_->setItem(row, 0, new QStandardItem(
            QString::fromStdString(r.dataset_code)));

        auto* statusItem = new QStandardItem(
            QString::fromStdString(r.status));
        if (r.status == "success") {
            statusItem->setForeground(QBrush(QColor("#228B22")));
        } else if (r.status == "failed") {
            statusItem->setForeground(QBrush(QColor("#cc0000")));
        }
        resultsModel_->setItem(row, 1, statusItem);

        auto* insertedItem = new QStandardItem(
            QString::number(r.records_inserted));
        insertedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsModel_->setItem(row, 2, insertedItem);

        auto* updatedItem = new QStandardItem(
            QString::number(r.records_updated));
        updatedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsModel_->setItem(row, 3, updatedItem);

        auto* skippedItem = new QStandardItem(
            QString::number(r.records_skipped));
        skippedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsModel_->setItem(row, 4, skippedItem);

        auto* deletedItem = new QStandardItem(
            QString::number(r.records_deleted));
        deletedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsModel_->setItem(row, 5, deletedItem);

        resultsModel_->setItem(row, 6, new QStandardItem(
            QString::fromStdString(r.error_message)));
    }

    resultsTable_->resizeColumnsToContents();
}

}
