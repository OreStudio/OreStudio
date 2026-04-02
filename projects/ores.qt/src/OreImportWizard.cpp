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
#include "ores.qt/OreImportWizard.hpp"

#include <set>
#include <QDate>
#include <QPalette>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QFileDialog>
#include <QFutureWatcher>
#include <QSizePolicy>
#include <QtConcurrent>
#include "ores.ore/scanner/ore_directory_scanner.hpp"
#include "ores.ore/planner/ore_import_planner.hpp"
#include "ores.ore/hierarchy/ore_hierarchy_builder.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.qt/OreImporter.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// OreImportWizard
// ============================================================================

OreImportWizard::OreImportWizard(ClientManager* clientManager,
                                 std::optional<boost::uuids::uuid> targetBookId,
                                 const std::string& targetBookName,
                                 QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager),
      targetBookName_(targetBookName) {

    setWindowTitle(tr("Import ORE Data"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BriefcaseFilled, IconUtils::DefaultIconColor));
    setMinimumSize(800, 600);
    resize(800, 600);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    // Default choices
    choices_.scan_exclusions  = {};          // scan all directories by default
    choices_.hierarchy_strip  = {"Input"};   // strip "Input" from hierarchy paths
    choices_.create_parent_portfolio = false;
    choices_.currency_mode = ore::planner::currency_import_mode::missing_only;
    choices_.party_id = clientManager_->currentPartyId();

    // Pre-set target book context from Portfolio Explorer selection
    if (targetBookId)
        choices_.existing_target_book_id = targetBookId;

    setupPages();
}

void OreImportWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome,     new OreWelcomePage(this));
    setPage(Page_Directory,   new OreDirectoryPage(this));
    setPage(Page_ScanSummary, new OreScanSummaryPage(this));
    setPage(Page_Currency,    new OreCurrencyPage(this));
    setPage(Page_Portfolio,   new OrePortfolioPage(this));
    setPage(Page_TradeImport, new OreTradeImportPage(this));
    setPage(Page_Done,        new OreDonePage(this));
    setStartId(Page_Welcome);
}

// ============================================================================
// OreWelcomePage
// ============================================================================

OreWelcomePage::OreWelcomePage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome to the ORE Import Wizard"));
    setSubTitle(tr("This wizard imports currencies, portfolios, books, and "
                   "trades from an ORE example directory into OreStudio."));

    auto* layout = new QVBoxLayout(this);

    auto* label = new QLabel(
        tr("<p>The wizard will guide you through the following steps:</p>"
           "<ol>"
           "<li><b>Directory</b> — select the root ORE directory to import</li>"
           "<li><b>Scan Summary</b> — review detected files and configure exclusions</li>"
           "<li><b>Currencies</b> — choose whether to import all or only missing currencies</li>"
           "<li><b>Portfolio Hierarchy</b> — set an optional parent portfolio name</li>"
           "<li><b>Trade Import</b> — set trade defaults and execute the import</li>"
           "<li><b>Done</b> — review results</li>"
           "</ol>"
           "<p>Click <b>Next</b> to begin.</p>"),
        this);
    label->setWordWrap(true);
    layout->addWidget(label);
    layout->addStretch();
}

// ============================================================================
// OreDirectoryPage
// ============================================================================

OreDirectoryPage::OreDirectoryPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Select ORE Directory"));
    setSubTitle(tr("Choose the root directory containing the ORE data files to import."));

    auto* layout = new QVBoxLayout(this);

    auto* row = new QHBoxLayout;
    dirEdit_ = new QLineEdit(this);
    dirEdit_->setPlaceholderText(tr("Path to ORE directory..."));
    browseBtn_ = new QPushButton(tr("Browse…"), this);
    row->addWidget(dirEdit_);
    row->addWidget(browseBtn_);
    layout->addLayout(row);

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);  // indeterminate
    progressBar_->hide();
    layout->addWidget(progressBar_);

    layout->addStretch();

    connect(browseBtn_, &QPushButton::clicked, this, &OreDirectoryPage::onBrowseClicked);
    connect(dirEdit_, &QLineEdit::textChanged, this, [this](const QString&) {
        scanComplete_ = false;
        statusLabel_->clear();
        emit completeChanged();
    });
}

bool OreDirectoryPage::isComplete() const {
    // While scanning: keep Next disabled so the user can't re-trigger
    if (scanning_) return false;
    // After a successful scan: allow advancing
    if (scanComplete_) return true;
    // Path entered but not yet scanned: enable Next so the user can click it to start the scan
    return !dirEdit_->text().trimmed().isEmpty();
}

void OreDirectoryPage::onBrowseClicked() {
    const auto dir = QFileDialog::getExistingDirectory(
        this, tr("Select ORE Directory"), dirEdit_->text());
    if (!dir.isEmpty())
        dirEdit_->setText(dir);
}

bool OreDirectoryPage::validatePage() {
    if (scanComplete_)
        return true;
    if (!scanning_)
        startScan();
    return false;  // stay on page; completeChanged() re-enables Next when done
}

void OreDirectoryPage::startScan() {
    const auto path = dirEdit_->text().trimmed();
    if (path.isEmpty()) {
        statusLabel_->setText(tr("Please select a directory."));
        return;
    }

    scanning_ = true;
    emit completeChanged();  // disable Next while scan runs

    statusLabel_->setText(tr("Scanning…"));
    progressBar_->show();
    browseBtn_->setEnabled(false);
    dirEdit_->setEnabled(false);

    const std::filesystem::path root = path.toStdString();
    const auto& excl_vec = wizard_->choices().scan_exclusions;
    const std::unordered_set<std::string> scan_exclusions(excl_vec.begin(),
                                                           excl_vec.end());

    auto* watcher = new QFutureWatcher<ore::scanner::scan_result>(this);
    connect(watcher, &QFutureWatcher<ore::scanner::scan_result>::finished,
            this, &OreDirectoryPage::onScanFinished);

    watcher->setFuture(QtConcurrent::run([root, scan_exclusions]() {
        ore::scanner::ore_directory_scanner scanner(root, scan_exclusions);
        return scanner.scan();
    }));
}

void OreDirectoryPage::onScanFinished() {
    auto* watcher =
        static_cast<QFutureWatcher<ore::scanner::scan_result>*>(sender());
    if (!watcher) return;

    const auto result = watcher->result();
    watcher->deleteLater();

    scanning_ = false;
    progressBar_->hide();
    browseBtn_->setEnabled(true);
    dirEdit_->setEnabled(true);

    wizard_->setScanResult(result);

    const int currencies = static_cast<int>(result.currency_files.size());
    const int portfolios = static_cast<int>(result.portfolio_files.size());

    statusLabel_->setText(
        tr("Scan complete: %1 currency file(s), %2 portfolio file(s).")
        .arg(currencies).arg(portfolios));

    BOOST_LOG_SEV(lg(), info) << "Scan complete: " << currencies
                              << " currency, " << portfolios << " portfolio files";

    scanComplete_ = true;
    emit completeChanged();
}

// ============================================================================
// OreScanSummaryPage
// ============================================================================

OreScanSummaryPage::OreScanSummaryPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Scan Summary"));
    setSubTitle(tr("Review the detected files and configure directory exclusions."));

    auto* layout = new QVBoxLayout(this);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    layout->addWidget(summaryLabel_);

    auto* excGroup = new QGroupBox(tr("Excluded directories"), this);
    auto* excLayout = new QVBoxLayout(excGroup);

    exclusionList_ = new QListWidget(this);
    excLayout->addWidget(exclusionList_);

    auto* addRow = new QHBoxLayout;
    exclusionEdit_ = new QLineEdit(this);
    exclusionEdit_->setPlaceholderText(tr("Directory name to exclude…"));
    addBtn_    = new QPushButton(tr("Add"),    this);
    removeBtn_ = new QPushButton(tr("Remove"), this);
    addRow->addWidget(exclusionEdit_);
    addRow->addWidget(addBtn_);
    addRow->addWidget(removeBtn_);
    excLayout->addLayout(addRow);
    layout->addWidget(excGroup);

    hierarchyLabel_ = new QLabel(this);
    hierarchyLabel_->setWordWrap(true);
    layout->addWidget(hierarchyLabel_);

    connect(addBtn_,    &QPushButton::clicked, this, &OreScanSummaryPage::onExclusionAdded);
    connect(removeBtn_, &QPushButton::clicked, this, &OreScanSummaryPage::onExclusionRemoved);
}

void OreScanSummaryPage::initializePage() {
    // Populate exclusion list from current choices
    exclusionList_->clear();
    for (const auto& e : wizard_->choices().scan_exclusions)
        exclusionList_->addItem(QString::fromStdString(e));

    refreshSummary();
}

void OreScanSummaryPage::onExclusionAdded() {
    const auto text = exclusionEdit_->text().trimmed();
    if (text.isEmpty()) return;

    auto& excl = wizard_->choices().scan_exclusions;
    const auto s = text.toStdString();
    if (std::find(excl.begin(), excl.end(), s) == excl.end()) {
        excl.push_back(s);
        exclusionList_->addItem(text);
    }
    exclusionEdit_->clear();
    refreshSummary();
}

void OreScanSummaryPage::onExclusionRemoved() {
    const auto* item = exclusionList_->currentItem();
    if (!item) return;
    const auto s = item->text().toStdString();
    auto& excl = wizard_->choices().scan_exclusions;
    excl.erase(std::remove(excl.begin(), excl.end(), s), excl.end());
    delete exclusionList_->takeItem(exclusionList_->row(item));
    refreshSummary();
}

void OreScanSummaryPage::refreshSummary() {
    const auto& sr = wizard_->scanResult();
    summaryLabel_->setText(
        tr("Found <b>%1</b> currency file(s) and <b>%2</b> portfolio file(s) "
           "(%3 file(s) ignored).")
        .arg(sr.currency_files.size())
        .arg(sr.portfolio_files.size())
        .arg(sr.ignored_files.size()));

    // Rebuild hierarchy to show portfolio/book node count
    ore::hierarchy::ore_hierarchy_builder builder(
        sr.portfolio_files, sr.root,
        std::unordered_set<std::string>(wizard_->choices().hierarchy_strip.begin(),
                                        wizard_->choices().hierarchy_strip.end()));
    const auto nodes = builder.build();

    long portfolios = std::count_if(nodes.begin(), nodes.end(),
        [](const auto& n) {
            return n.type == ore::hierarchy::import_node::node_type::portfolio;
        });
    long books = static_cast<long>(nodes.size()) - portfolios;

    hierarchyLabel_->setText(
        tr("Hierarchy preview: <b>%1</b> portfolio(s), <b>%2</b> book(s).")
        .arg(portfolios).arg(books));
}

// ============================================================================
// OreCurrencyPage
// ============================================================================

OreCurrencyPage::OreCurrencyPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Currency Import"));
    setSubTitle(tr("Choose which currencies to import."));

    auto* layout = new QVBoxLayout(this);

    allRadio_ = new QRadioButton(
        tr("Import all currencies from the ORE file"), this);
    missingRadio_ = new QRadioButton(
        tr("Import only currencies not already present in OreStudio"), this);
    missingRadio_->setChecked(true);

    layout->addWidget(allRadio_);
    layout->addWidget(missingRadio_);

    statusLabel_ = new QLabel(tr("Fetching existing currencies…"), this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    countLabel_ = new QLabel(this);
    layout->addWidget(countLabel_);

    layout->addStretch();

    connect(allRadio_,     &QRadioButton::toggled, this, &OreCurrencyPage::onModeChanged);
    connect(missingRadio_, &QRadioButton::toggled, this, &OreCurrencyPage::onModeChanged);
}

void OreCurrencyPage::initializePage() {
    fetchDone_ = false;
    statusLabel_->setText(tr("Fetching existing currencies from server…"));
    countLabel_->clear();

    auto* cm = wizard_->clientManager();

    // Fetch all existing ISO codes asynchronously
    using Result = std::set<std::string>;
    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, &OreCurrencyPage::onFetchFinished);

    watcher->setFuture(QtConcurrent::run([cm]() -> Result {
        refdata::messaging::get_currencies_request req;
        req.limit = 1000;  // server enforces max 1000 per request
        const auto resp = cm->process_authenticated_request(std::move(req));
        Result codes;
        if (resp) {
            for (const auto& c : resp->currencies)
                codes.insert(c.iso_code);
        }
        return codes;
    }));
}

void OreCurrencyPage::onFetchFinished() {
    auto* watcher = static_cast<QFutureWatcher<std::set<std::string>>*>(sender());
    if (!watcher) return;

    wizard_->setExistingIsoCodes(watcher->result());
    watcher->deleteLater();

    const int existing = static_cast<int>(wizard_->existingIsoCodes().size());
    statusLabel_->setText(tr("Server has %1 existing currencies.").arg(existing));

    fetchDone_ = true;
    onModeChanged();
}

void OreCurrencyPage::onModeChanged() {
    using mode = ore::planner::currency_import_mode;
    wizard_->choices().currency_mode =
        missingRadio_->isChecked() ? mode::missing_only : mode::all;

    if (!fetchDone_) return;

    // Compute how many will be imported with current mode
    const auto& sr = wizard_->scanResult();
    const auto& existing = wizard_->existingIsoCodes();
    const auto& choices = wizard_->choices();

    ore::planner::ore_import_planner planner(sr, existing, choices);
    const auto plan = planner.plan();

    countLabel_->setText(
        tr("This will import %1 currency record(s).")
        .arg(plan.currencies.size()));
}

// ============================================================================
// OrePortfolioPage
// ============================================================================

OrePortfolioPage::OrePortfolioPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Import Target"));
    setSubTitle(tr("Select the book that trades will be imported into."));

    auto* layout = new QVBoxLayout(this);

    auto* bookRow = new QHBoxLayout;
    bookRow->addWidget(new QLabel(tr("Target book:"), this));
    parentCombo_ = new QComboBox(this);
    parentCombo_->setEditable(false);
    parentCombo_->setPlaceholderText(tr("Loading books…"));
    bookRow->addWidget(parentCombo_);
    layout->addLayout(bookRow);

    hierarchyPreviewLabel_ = new QLabel(this);
    hierarchyPreviewLabel_->setWordWrap(true);
    layout->addWidget(hierarchyPreviewLabel_);

    layout->addStretch();

    connect(parentCombo_, &QComboBox::currentTextChanged,
            this, &OrePortfolioPage::onBookSelectionChanged);
}

void OrePortfolioPage::initializePage() {
    fetchDone_ = false;
    booksByName_.clear();
    parentCombo_->clear();
    parentCombo_->setEnabled(false);
    hierarchyPreviewLabel_->setText(tr("Loading available books…"));

    auto* cm = wizard_->clientManager();
    using Result = std::vector<refdata::domain::book>;
    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, &OrePortfolioPage::onBooksFetchFinished);

    watcher->setFuture(QtConcurrent::run([cm]() -> Result {
        refdata::messaging::get_books_request req;
        req.limit = 1000;
        const auto resp = cm->process_authenticated_request(std::move(req));
        Result books;
        if (resp) {
            books = resp->books;
            std::sort(books.begin(), books.end(),
                      [](const auto& a, const auto& b) { return a.name < b.name; });
        }
        return books;
    }));
}

bool OrePortfolioPage::isComplete() const {
    if (!fetchDone_)
        return false;
    const auto name = parentCombo_->currentText().toStdString();
    return booksByName_.count(name) > 0;
}

bool OrePortfolioPage::validatePage() {
    const auto name = parentCombo_->currentText().toStdString();
    const auto it = booksByName_.find(name);
    if (it == booksByName_.end())
        return false;
    wizard_->choices().existing_target_book_id      = it->second.id;
    wizard_->choices().existing_parent_portfolio_id = it->second.parent_portfolio_id;
    return true;
}

void OrePortfolioPage::onBooksFetchFinished() {
    auto* watcher = static_cast<QFutureWatcher<std::vector<refdata::domain::book>>*>(sender());
    if (!watcher) return;

    const auto books = watcher->result();
    watcher->deleteLater();

    BOOST_LOG_SEV(lg(), info) << "Fetched " << books.size() << " existing books";

    for (const auto& b : books)
        booksByName_[b.name] = b;

    parentCombo_->clear();
    for (const auto& b : books)
        parentCombo_->addItem(QString::fromStdString(b.name));

    // Pre-select: context book (from Explorer) > empty
    const auto& contextName = wizard_->targetBookName();
    if (!contextName.empty())
        parentCombo_->setCurrentText(QString::fromStdString(contextName));

    parentCombo_->setEnabled(true);
    fetchDone_ = true;
    emit completeChanged();
    onBookSelectionChanged();
}

void OrePortfolioPage::onBookSelectionChanged() {
    const auto name = parentCombo_->currentText().toStdString();
    const auto it = booksByName_.find(name);
    if (it == booksByName_.end()) {
        hierarchyPreviewLabel_->setText(tr("Select a book to see import details."));
        emit completeChanged();
        return;
    }
    const auto& book = it->second;
    // Count total trades from hierarchy
    const auto& sr = wizard_->scanResult();
    const auto& choices = wizard_->choices();
    ore::hierarchy::ore_hierarchy_builder builder(
        sr.portfolio_files, sr.root,
        std::unordered_set<std::string>(choices.hierarchy_strip.begin(),
                                        choices.hierarchy_strip.end()));
    const auto nodes = builder.build();
    long total_files = std::count_if(nodes.begin(), nodes.end(),
        [](const auto& n) {
            return n.type == ore::hierarchy::import_node::node_type::book;
        });
    hierarchyPreviewLabel_->setText(
        tr("Trades from %1 portfolio file(s) will be imported into book \"%2\".")
        .arg(total_files)
        .arg(QString::fromStdString(book.name)));
    emit completeChanged();
}

// ============================================================================
// OreTradeImportPage
// ============================================================================

OreTradeImportPage::OreTradeImportPage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Trade Import"));
    setSubTitle(tr("Set trade defaults and execute the import."));

    auto* layout = new QVBoxLayout(this);

    auto* form = new QFormLayout;
    tradeDateEdit_ = new QLineEdit(this);
    tradeDateEdit_->setPlaceholderText(tr("YYYY-MM-DD (leave blank to keep ORE value)"));
    form->addRow(tr("Default trade date:"), tradeDateEdit_);

    lifecycleEventCombo_ = new QComboBox(this);
    lifecycleEventCombo_->addItem(tr("(keep ORE value)"), QString());
    lifecycleEventCombo_->setEnabled(false);
    form->addRow(tr("Default lifecycle event:"), lifecycleEventCombo_);

    defaultCounterpartyCombo_ = new QComboBox(this);
    defaultCounterpartyCombo_->addItem(tr("-- None (use ORE value) --"), QString());
    defaultCounterpartyCombo_->setEnabled(false);
    counterpartyStatusLabel_ = new QLabel(tr("Loading…"), this);
    counterpartyStatusLabel_->setStyleSheet(QStringLiteral("color: gray; font-style: italic;"));
    auto* cpRow = new QHBoxLayout;
    cpRow->addWidget(defaultCounterpartyCombo_);
    cpRow->addWidget(counterpartyStatusLabel_);
    form->addRow(tr("Default counterparty:"), cpRow);

    layout->addLayout(form);

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 4);
    progressBar_->hide();
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    logOutput_->document()->setDefaultFont(FontUtils::monospace());
    logOutput_->hide();
    layout->addWidget(logOutput_);
}

bool OreTradeImportPage::isComplete() const {
    if (importDone_)    return true;   // allow advancing after import
    if (importStarted_) return false;  // disable Next while running
    return true;                       // enable Next to trigger import
}

void OreTradeImportPage::initializePage() {
    importDone_    = false;
    importStarted_ = false;
    statusLabel_->setText(tr("Click Next to start the import."));
    progressBar_->hide();
    logOutput_->hide();
    logOutput_->clear();

    // Pre-fill default trade date
    if (tradeDateEdit_->text().isEmpty())
        tradeDateEdit_->setText(QDate::currentDate().toString("yyyy-MM-dd"));

    auto* cm = wizard_->clientManager();

    // Async fetch activity types to populate lifecycle event combo
    lifecycleEventCombo_->setEnabled(false);
    using ActivityTypeList = std::vector<trading::domain::activity_type>;
    auto* atWatcher = new QFutureWatcher<ActivityTypeList>(this);
    connect(atWatcher, &QFutureWatcher<ActivityTypeList>::finished,
            this, &OreTradeImportPage::onActivityTypesFetchFinished);
    atWatcher->setFuture(QtConcurrent::run([cm]() -> ActivityTypeList {
        trading::messaging::get_activity_types_request req;
        const auto resp = cm->process_authenticated_request(std::move(req));
        if (!resp) return {};
        return resp->activity_types;
    }));

    // Async fetch counterparties to populate the default counterparty combo
    defaultCounterpartyCombo_->setEnabled(false);
    counterpartyStatusLabel_->setText(tr("Loading…"));

    using CounterpartyList = std::vector<refdata::domain::counterparty>;
    auto* cpWatcher = new QFutureWatcher<CounterpartyList>(this);
    connect(cpWatcher, &QFutureWatcher<CounterpartyList>::finished,
            this, &OreTradeImportPage::onCounterpartiesFetchFinished);

    cpWatcher->setFuture(QtConcurrent::run([cm]() -> CounterpartyList {
        refdata::messaging::get_counterparties_request req;
        req.limit = 1000;
        const auto resp = cm->process_authenticated_request(std::move(req));
        if (!resp) return {};
        return resp->counterparties;
    }));
}

void OreTradeImportPage::onActivityTypesFetchFinished() {
    using ActivityTypeList = std::vector<trading::domain::activity_type>;
    auto* watcher = static_cast<QFutureWatcher<ActivityTypeList>*>(sender());
    if (!watcher) return;

    const auto types = watcher->result();
    watcher->deleteLater();

    BOOST_LOG_SEV(lg(), info) << "Fetched " << types.size() << " activity types";

    // Preserve current selection across re-visits
    const QString prevCode = lifecycleEventCombo_->currentData().toString();
    lifecycleEventCombo_->clear();
    lifecycleEventCombo_->addItem(tr("(keep ORE value)"), QString());

    for (const auto& t : types) {
        const QString code = QString::fromStdString(t.code);
        const QString label = t.description.empty()
            ? code
            : QString("%1 (%2)").arg(QString::fromStdString(t.description), code);
        lifecycleEventCombo_->addItem(label, code);
    }

    // Restore prior selection; otherwise default to new_booking
    bool restored = false;
    if (!prevCode.isEmpty()) {
        for (int i = 1; i < lifecycleEventCombo_->count(); ++i) {
            if (lifecycleEventCombo_->itemData(i).toString() == prevCode) {
                lifecycleEventCombo_->setCurrentIndex(i);
                restored = true;
                break;
            }
        }
    }
    if (!restored) {
        for (int i = 1; i < lifecycleEventCombo_->count(); ++i) {
            if (lifecycleEventCombo_->itemData(i).toString() == QStringLiteral("new_booking")) {
                lifecycleEventCombo_->setCurrentIndex(i);
                break;
            }
        }
    }

    lifecycleEventCombo_->setEnabled(true);
}

void OreTradeImportPage::onCounterpartiesFetchFinished() {
    using CounterpartyList = std::vector<refdata::domain::counterparty>;
    auto* watcher = static_cast<QFutureWatcher<CounterpartyList>*>(sender());
    if (!watcher) return;

    const auto counterparties = watcher->result();
    watcher->deleteLater();

    BOOST_LOG_SEV(lg(), info) << "Fetched " << counterparties.size()
                              << " counterparties for import defaults";

    // Preserve current selection across re-visits
    const QString prevData = defaultCounterpartyCombo_->currentData().toString();
    defaultCounterpartyCombo_->clear();
    defaultCounterpartyCombo_->addItem(tr("-- None (use ORE value) --"), QString());

    for (const auto& cp : counterparties) {
        defaultCounterpartyCombo_->addItem(
            QString::fromStdString(cp.full_name),
            QString::fromStdString(boost::uuids::to_string(cp.id)));
    }

    if (!prevData.isEmpty()) {
        for (int i = 1; i < defaultCounterpartyCombo_->count(); ++i) {
            if (defaultCounterpartyCombo_->itemData(i).toString() == prevData) {
                defaultCounterpartyCombo_->setCurrentIndex(i);
                break;
            }
        }
    }

    if (counterparties.empty()) {
        counterpartyStatusLabel_->setText(tr("(no counterparties found)"));
    } else {
        counterpartyStatusLabel_->setText(
            tr("(%1 loaded)").arg(static_cast<int>(counterparties.size())));
    }
    defaultCounterpartyCombo_->setEnabled(true);
}

bool OreTradeImportPage::validatePage() {
    if (importDone_) return true;
    if (!importStarted_) startImport();
    return false;
}

void OreTradeImportPage::appendLog(const QString& msg) {
    logOutput_->append(msg);
    BOOST_LOG_SEV(lg(), info) << msg.toStdString();
}

void OreTradeImportPage::startImport() {
    importStarted_ = true;
    emit completeChanged();  // disable Next while import runs

    // Capture defaults from form
    auto& defs = wizard_->choices().defaults;
    defs.trade_date      = tradeDateEdit_->text().trimmed().toStdString();
    defs.activity_type_code = lifecycleEventCombo_->currentData().toString().toStdString();

    const QString cpUuidStr = defaultCounterpartyCombo_->currentData().toString();
    if (!cpUuidStr.isEmpty()) {
        try {
            defs.default_counterparty_id =
                boost::lexical_cast<boost::uuids::uuid>(cpUuidStr.toStdString());
        } catch (...) {
            defs.default_counterparty_id = std::nullopt;
        }
    } else {
        defs.default_counterparty_id = std::nullopt;
    }

    progressBar_->setValue(0);
    progressBar_->show();
    logOutput_->show();
    statusLabel_->setText(tr("Building import plan…"));

    // Build plan on background thread
    const auto sr       = wizard_->scanResult();
    const auto existing = wizard_->existingIsoCodes();
    const auto choices  = wizard_->choices();

    auto* cm = wizard_->clientManager();

    using Result = ore::planner::ore_import_result;
    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished,
            this, &OreTradeImportPage::onImportFinished);

    watcher->setFuture(QtConcurrent::run([sr, existing, choices, cm]() -> Result {
        OreImporter importer(cm);
        return importer.execute(sr, existing, choices);
    }));
}

void OreTradeImportPage::onImportFinished() {
    using Result = ore::planner::ore_import_result;
    auto* fw = static_cast<QFutureWatcher<Result>*>(sender());
    if (!fw) return;

    const auto res = fw->result();
    fw->deleteLater();

    progressBar_->setValue(4);
    wizard_->setImportSuccess(res.success);
    wizard_->setImportError(QString::fromStdString(res.error));

    if (res.success) {
        appendLog(tr("Saved %1 currencies.").arg(res.currencies));
        appendLog(tr("Saved %1 portfolios.").arg(res.portfolios));
        appendLog(tr("Saved %1 books.").arg(res.books));
        appendLog(tr("Saved %1 trades.").arg(res.trades));

        const int failed = static_cast<int>(res.instrument_errors.size());
        if (failed == 0) {
            appendLog(tr("Saved %1 instruments.").arg(res.instruments));
        } else {
            appendLog(tr("Saved %1 instruments, %2 failed:").arg(res.instruments).arg(failed));
            for (const auto& err : res.instrument_errors)
                appendLog(tr("  [instrument error] %1: %2")
                    .arg(QString::fromStdString(err.trade_external_id),
                         QString::fromStdString(err.message)));
        }

        if (failed == 0) {
            statusLabel_->setText(tr("Import completed successfully."));
        } else {
            statusLabel_->setText(
                tr("Import completed with %1 instrument failure(s). "
                   "See the log below for details.").arg(failed));
        }

        wizard_->setImportResults(res.currencies, res.portfolios, res.books, res.trades,
                                  res.instruments, failed);

        BOOST_LOG_SEV(lg(), info) << "ORE import complete: "
            << res.currencies << " currencies, "
            << res.portfolios << " portfolios, "
            << res.books << " books, "
            << res.trades << " trades, "
            << res.instruments << " instruments saved, "
            << failed << " instruments failed";
        importDone_ = true;
        emit completeChanged();
    } else {
        appendLog(tr("Error: %1").arg(QString::fromStdString(res.error)));
        statusLabel_->setText(tr("Import failed. See the error log below. "
                                  "Click Cancel to abort the wizard."));
        BOOST_LOG_SEV(lg(), error) << "ORE import failed: " << res.error;

        // Turn progress bar red to signal failure
        QPalette pal = progressBar_->palette();
        pal.setColor(QPalette::Highlight, Qt::red);
        progressBar_->setPalette(pal);
        // importDone_ stays false — user must cancel rather than advance
    }
}

// ============================================================================
// OreDonePage
// ============================================================================

OreDonePage::OreDonePage(OreImportWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Import Complete"));
    setSubTitle(tr("The ORE import has finished."));

    auto* layout = new QVBoxLayout(this);
    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    layout->addWidget(summaryLabel_);
    layout->addStretch();
}

void OreDonePage::initializePage() {
    if (wizard_->importSuccess()) {
        const int failed = wizard_->savedInstrumentFailures();
        const QString instrLine = failed == 0
            ? tr("<li>Instruments: %1</li>").arg(wizard_->savedInstruments())
            : tr("<li>Instruments: %1 saved, <b>%2 failed</b> — see import log for details</li>")
                .arg(wizard_->savedInstruments()).arg(failed);
        const QString heading = failed == 0
            ? tr("<p><b>Import succeeded.</b></p>")
            : tr("<p><b>Import completed with instrument failures.</b></p>");
        summaryLabel_->setText(
            heading +
            tr("<ul>"
               "<li>Currencies: %1</li>"
               "<li>Portfolios: %2</li>"
               "<li>Books: %3</li>"
               "<li>Trades: %4</li>")
            .arg(wizard_->savedCurrencies())
            .arg(wizard_->savedPortfolios())
            .arg(wizard_->savedBooks())
            .arg(wizard_->savedTrades())
            + instrLine +
            tr("</ul>"
               "<p>The data is now available in the Portfolio Explorer.</p>"));
    } else {
        summaryLabel_->setText(
            tr("<p><b>Import failed.</b></p><p>%1</p>")
            .arg(wizard_->importError().isEmpty()
                 ? tr("Unknown error") : wizard_->importError()));
    }
}

}
